{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


-- |This module is the main backing for FSMs. Instances are stored in a table
-- with the name passed as storeName when creating the PostgresJSONStore. WALs use
-- the same name with "Wal" appended.
-- This store persists FSMs stored as key/value in postgresql, where keys are uuids
-- and values are JSONB fields.
module PostgresJSONStore(
    PostgresJSONStore,
    mkStore
) where


import           Control.Monad (liftM, void)
import           Database.PostgreSQL.Simple as PGS
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Transaction
import           Database.PostgreSQL.Simple.Types
import           Data.Aeson
import qualified Data.ByteString.Char8 as DBSC8
import           Data.Int (Int64)
import           Data.Maybe (maybe, listToMaybe)
import           Data.Pool
import           Data.Text
import           Data.Time
import           Data.Typeable
import           Data.UUID
import           GHC.Generics

import FSM
import FSMStore
import WALStore

data PostgresJSONStore = PostgresJSONStore {
    storeConnPool :: Pool Connection,
    storeName     :: Text
}

instance (FromJSON s, FromJSON e, FromJSON a,
          ToJSON   s, ToJSON   e, ToJSON   a,
          Typeable s, Typeable e, Typeable a) => FSMStore PostgresJSONStore (Instance s e a) where
    fsmRead            = PostgresJSONStore.fsmRead
    fsmUpdate          = PostgresJSONStore.fsmUpdate
    fsmCreate          = PostgresJSONStore.fsmCreate


instance WALStore PostgresJSONStore where
    walUpsertIncrement = PostgresJSONStore.walUpsertIncrement
    walDecrement       = PostgresJSONStore.walDecrement
    walScan            = PostgresJSONStore.walScan



givePool :: IO Connection -> IO (Pool Connection)
givePool creator = createPool creator close 1 (10 * 10^12) 20


-- #########
-- # FSM API
-- #########
fsmRead :: (FromJSON s, FromJSON e, FromJSON a,
            Typeable s, Typeable e, Typeable a) =>
            PostgresJSONStore                   ->
            UUID                                -> IO (Maybe (Instance s e a))
fsmRead st k =
    withResource (storeConnPool st) (\conn ->
        withTransactionSerializable conn $ do
            el <- _getValue conn (storeName st) k
            return $ listToMaybe el)


fsmCreate :: (ToJSON   s, ToJSON   e, ToJSON   a,
              Typeable s, Typeable e, Typeable a) =>
              PostgresJSONStore                   ->
              Instance s e a                      -> IO ()
fsmCreate st i =
    withResource (storeConnPool st) (\conn ->
        withTransactionSerializable conn $
            void $ _postValue conn (storeName st) (uuid i) (machine i))


-- |Postgresql-simple exceptions will be caught by `patch` in FSMApi.hs
fsmUpdate :: (FromJSON s, FromJSON e, FromJSON a,
              ToJSON   s, ToJSON   e, ToJSON   a,
              Typeable s, Typeable e, Typeable a) =>
              PostgresJSONStore                   ->
              UUID                                ->
              Transformer s e a                   -> IO OutboxStatus
fsmUpdate st i t =
    withResource (storeConnPool st) (\conn ->
        withTransactionSerializable conn $ do
            el    <- _getValueForUpdate conn (storeName st) i
            let entry = listToMaybe el
            maybe
                (return NotFound)
                (\e -> do
                    newMachine <- liftM machine (t e)
                    void (_postValue conn (storeName st) i newMachine)
                    return $ if Prelude.null (outbox newMachine) then Done else Pending)
                entry)

-- #####
-- # WAL
-- #####
_createWalTable :: Connection -> Text -> IO Int64
_createWalTable conn name =
    PGS.execute conn "CREATE TABLE IF NOT EXISTS ? ( id uuid PRIMARY KEY, date timestamptz NOT NULL, count int NOT NULL )" (Only (Identifier name))

-- |Updates a WALEntry if it exists, inserts a new WALEntry if is is missing.
walUpsertIncrement st i =
    _walExecute st i _walIncrement
walDecrement st i =
    _walExecute st i _walDecrement

_walExecute :: PostgresJSONStore -> UUID -> Query -> IO ()
_walExecute st i q = let tbl = append (storeName st) "Wal" in
    withResource (storeConnPool st) (\conn ->
        withTransactionSerializable conn $ do
            now   <- getCurrentTime
            void $ PGS.execute conn q (Identifier tbl, i, now, Identifier tbl))

_walIncrement :: Query
_walIncrement = "INSERT INTO ? VALUES (?,?,1) ON CONFLICT (id) DO UPDATE SET count = ?.count + 1, date = EXCLUDED.date"

_walDecrement :: Query
_walDecrement = "INSERT INTO ? VALUES (?,?,0) ON CONFLICT (id) DO UPDATE SET count = ?.count - 1"


-- |Returns a list of all transactions that were not successfully terminated
-- and are older than `cutoff`.
walScan :: PostgresJSONStore -> Int -> IO [WALEntry]
walScan st cutoff = do
    t <- getCurrentTime
    let xx = addUTCTime (negate (fromInteger (toInteger cutoff) :: NominalDiffTime)) t

    withResource (storeConnPool st) (\c ->
        withTransactionSerializable c $
            PGS.query c "SELECT * FROM ? WHERE date < ? AND count > 0" (Identifier $ append (storeName st) "Wal", xx))

-- |Creates a postgresql store
mkStore :: String -> Text -> IO PostgresJSONStore
mkStore connStr name =
    let
        connBS = DBSC8.pack connStr
    in do
        pool <- givePool (PGS.connectPostgreSQL connBS)
        withResource pool $ flip _createFsmTable name
        withResource pool $ flip _createWalTable (append name "Wal")
        return $ PostgresJSONStore pool name

_createFsmTable :: Connection -> Text -> IO Int64
_createFsmTable conn name =
    PGS.execute conn "CREATE TABLE IF NOT EXISTS ? ( id uuid PRIMARY KEY, data jsonb NOT NULL)" (Only (Identifier name))

-- |A brief lesson on transaction isolation:
-- According to the SQL standard Serializable is the *only* isolation level
-- that offers protection against concurrent updates, which is something we need.
-- If performance is a problem you may want to lower the isolation level to
-- RepeatableRead, which in PostgreSQL actually is the non-standard
-- technique/isolation level Snapshot Isolation that is sufficient to
-- protect against concurrent updates.
-- In doing so you must keep two things in mind:
-- * You must still be prepared to retry transactions, when they fail due to
--   concurrent updates.
-- * Repeatable Read, or in PostgreSQL's case Snapshot Isolation, does *not* protect
--   against write skew, which means that if you read from the table in a (phase2)
--   handler, and receive an empty result, a concurrent transaction may insert a new Instance
--   so that if you later retried the read you would now receive a non-empty result.
--   If you are not careful you may end up with wrong data or attempts to insert data
--   with a duplicate ID resulting in an exception,…
--   Hence, when in doubt, do not lower the isolation level.
-- The one even lower isolation level in PostgreSQL, ReadCommitted, does not
-- protect against concurrent updates and is therefore not suitable.

-- HOWEVER: (disregard everything above)
-- Since it is a priority to execute effects only once, as good as we can, this changes
-- the picture. SELECT FOR UPDATE locks the rows matching the query. Concurrent transactions
-- will block and then abort, once the new value has been inserted. Since we run effects
-- between SELECT and INSERT, this is what we want.
-- Concurrent SELECTS (without FOR UPDATE) will be unaffected.
_getValue :: (FromRow v, ToField k) => Connection -> Text -> k -> IO [v]
_getValue c tbl k =
    PGS.query c "SELECT * FROM ? WHERE id = ?" (Identifier tbl, k)

_getValueForUpdate :: (FromRow v, ToField k) => Connection -> Text -> k -> IO [v]
_getValueForUpdate c tbl k =
    PGS.query c "SELECT * FROM ? WHERE id = ? FOR UPDATE" (Identifier tbl, k)

_postValue :: (ToField k, ToField v) => Connection -> Text -> k -> v -> IO Int64
_postValue c tbl k v =
    PGS.execute c "INSERT INTO ? VALUES (?,?) ON CONFLICT (id) DO UPDATE SET data = EXCLUDED.data" (Identifier tbl, k, v)

_deleteValue :: (ToField k) => Connection -> Text -> k -> IO Int64
_deleteValue c tbl k =
    PGS.execute c "DELETE FROM ? WHERE id = ?" (Identifier tbl, k)

_queryValue :: (FromRow v) => Connection -> Text -> String -> IO [v]
_queryValue c tbl q =
    PGS.query c "SELECT * FROM ? WHERE data @> ?" (Identifier tbl, q)


-- |Instance to convert one DB row to an instance of Instance ;)
-- users of this module must provide instances for ToJSON, FromJSON for `s`, `e` and `a`.
instance (ToJSON s,   ToJSON e,   ToJSON a)   => ToJSON (Machine s e a)
instance (FromJSON s, FromJSON e, FromJSON a) => FromJSON (Machine s e a)

instance (ToJSON e)   => ToJSON (Msg e)
instance (FromJSON e) => FromJSON (Msg e)

instance (Typeable s, Typeable e, Typeable a,
          FromJSON s, FromJSON e, FromJSON a) => FromRow (Instance s e a) where
    fromRow = Instance <$> field <*> field

instance (Typeable s, Typeable e, Typeable a,
          FromJSON s, FromJSON e, FromJSON a) => FromField (Machine s e a) where
    fromField = fromJSONField

instance (Typeable s, Typeable e, Typeable a,
          ToJSON s, ToJSON e, ToJSON a) => ToField (Machine s e a) where
    toField = toJSONField

instance FromRow WALEntry where
    fromRow = WALEntry <$> field <*> field <*> field

deriving instance Generic  WALEntry
deriving instance Typeable WALEntry
deriving instance ToJSON   WALEntry
deriving instance FromJSON WALEntry
