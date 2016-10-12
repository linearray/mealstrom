{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module PostgresqlStore(
    PostgresqlStore,
    StoreType(..),
    OutboxStatus(..),
    createFsmStore,
    createWalStore,
    fsmCreate,
    fsmRead,
    fsmUpdate,
    walScan,
    walUpsert
) where

-- |This store persists FSMs stored as key/value in postgresql, where keys are uuids
-- and values are JSONB fields.

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

import FSM

data OutboxStatus = NotFound | Pending | Done deriving (Eq,Show)

data StoreType = WAL | FSM

data PostgresqlStore (t :: StoreType) = PostgresqlStore {
    storeConnPool :: Pool Connection,
    storeName     :: Text
}

givePool :: IO Connection -> IO (Pool Connection)
givePool creator = createPool creator close 1 (10 * 10^12) 20


-- #########
-- # FSM API
-- #########
fsmRead :: (FromJSON s, FromJSON e, FromJSON a,
            Typeable s, Typeable e, Typeable a) =>
            PostgresqlStore FSM                 ->
            UUID                                -> IO (Maybe (Instance s e a))
fsmRead st k =
    withResource (storeConnPool st) (\conn ->
        withTransactionSerializable conn $ do
            el <- _getValue conn (storeName st) k
            return $ listToMaybe el)


fsmCreate :: (ToJSON s, ToJSON e, ToJSON a,
              Typeable s, Typeable e, Typeable a) =>
              PostgresqlStore FSM                 ->
              Instance s e a                      -> IO ()
fsmCreate st i =
    withResource (storeConnPool st) (\conn ->
        withTransactionSerializable conn $
            void $ _postValue conn (storeName st) (uuid i) (machine i))


-- |Postgresql-simple exceptions will be caught by `patch` in FSMApi.hs

fsmUpdate :: (FromJSON s, FromJSON e, FromJSON a,
              ToJSON s, ToJSON e, ToJSON a,
              Typeable s, Typeable e, Typeable a) =>
              PostgresqlStore FSM                 ->
              UUID                                ->
              Transformer s e a                   -> IO OutboxStatus

fsmUpdate st i t =
    withResource (storeConnPool st) (\conn ->
        withTransactionSerializable conn $ do
            el    <- _getValue conn (storeName st) i
            let entry = listToMaybe el
            maybe
                (return NotFound)
                (\e -> do
                    newMachine <- t (machine e)
                    void (_postValue conn (storeName st) i newMachine)
                    return $ if Prelude.null (outbox newMachine) then Done else Pending)
                entry)

-- #####
-- # WAL
-- #####
createWalStore :: String -> Text -> IO (PostgresqlStore WAL)
createWalStore connStr name =
    let
        connBS = DBSC8.pack connStr
    in do
        pool  <- givePool (PGS.connectPostgreSQL connBS)
        withResource pool $ flip _createWalTable name
        return (PostgresqlStore pool name :: PostgresqlStore WAL)

_createWalTable :: Connection -> Text -> IO Int64
_createWalTable conn name =
    PGS.execute conn "CREATE TABLE IF NOT EXISTS ? ( id uuid PRIMARY KEY, date timestamptz NOT NULL, count int NOT NULL )" (Only (Identifier name))

-- |Updates a WALEntry if it exists, inserts a new WALEntry if is is missing.
-- We could improve performance by doing something like
-- INSERT ... ON CONFLICT UPDATE SET count = count +/- 1;
walUpsert :: PostgresqlStore WAL -> UUID -> (Int -> Int) -> IO ()
walUpsert st i t =
    withResource (storeConnPool st) (\conn ->
        withTransactionSerializable conn $ do
            now   <- getCurrentTime
            entry <- _walRead conn i

            maybe
                (_walWrite conn (WALEntry i now 1))
                (\(WALEntry i tm n) -> _walWrite conn (WALEntry i tm $ t n))
                entry)
  where
    _walRead :: Connection -> UUID -> IO (Maybe WALEntry)
    _walRead conn k = do
        el <- _getValue conn (storeName st) k
        return $ listToMaybe el

    _walWrite :: Connection -> WALEntry -> IO ()
    _walWrite conn (WALEntry i t n) =
        void $ PGS.execute conn "INSERT INTO ? VALUES (?,?,?) ON CONFLICT (id) DO UPDATE SET count = EXCLUDED.count"
            (Identifier $ storeName st, i,t,n)

-- |Returns a list of all transactions that were not successfully terminated
-- and are older than `cutoff`.
walScan :: PostgresqlStore WAL -> Integer -> IO [WALEntry]
walScan st cutoff = do
    t <- getCurrentTime
    let xx = addUTCTime (negate (fromInteger (cutoff * 10^12) :: NominalDiffTime)) t
    let nt = diffUTCTime t xx

    withResource (storeConnPool st) (\c ->
        withTransactionSerializable c $
            PGS.query c "SELECT * FROM ? WHERE date < ? AND count > 0" (Identifier $ storeName st, nt))

-- |Creates a postgresql store
createFsmStore :: String -> Text -> IO (PostgresqlStore FSM)
createFsmStore connStr name =
    let
        connBS = DBSC8.pack connStr
    in do
        pool <- givePool (PGS.connectPostgreSQL connBS)
        withResource pool $ flip _createFsmTable name
        return (PostgresqlStore pool name :: PostgresqlStore FSM)

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
_getValue :: (FromRow v, ToField k) => Connection -> Text -> k -> IO [v]
_getValue c tbl k =
    PGS.query c "SELECT * FROM ? WHERE id = ?" (Identifier tbl, k)

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
