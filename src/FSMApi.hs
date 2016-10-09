{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}


module FSMApi where

import FSM
import FSMEngine
import FSMTable
import PostgresqlStore
import WAL

import Control.Concurrent
import Control.Monad (liftM,forM,void)
import Data.Aeson
import Data.Foldable (forM_)
import Data.Maybe
import Data.Text
import Data.Time
import Data.Typeable
import Data.UUID
import GHC.Generics


-- |API for FSMs.
-- This is the interface through which you can interact with a FSM
-- from the rest of your program.
data FSMHandle s e a = (ToJSON   s, ToJSON   e, ToJSON   a,
                        FromJSON s, FromJSON e, FromJSON a,
                        Typeable s, Typeable e, Typeable a) => FSMHandle {
    fsmName        :: Text,
    fsmTable       :: FSMTable s e a,
    fsmStore       :: PostgresqlStore FSM,
    walStore       :: PostgresqlStore WAL,
    walExpiration  :: Integer                -- how old must a pending or crashed transaction be
                                             -- for us to retry.
}

get :: forall s e a . (FromJSON s, FromJSON e, FromJSON a,
                       Typeable s, Typeable e, Typeable a,
                       Eq s, Eq e, Eq a)                   =>

                       FSMHandle s e a                     ->
                       UUID                                -> IO (Maybe s)
get h@FSMHandle{..} i = fmap (fmap (evalState fsmTable . machine)) res
  where
      res = fsmRead fsmStore i :: IO (Maybe (Instance s e a))


-- |Idempotent because of usage of caller-generated UUIDs
-- FIXME: This can throw an exception.
post :: forall s e a . (ToJSON   s, ToJSON   e, ToJSON   a,
                        Typeable s, Typeable e, Typeable a) =>

                        FSMHandle s e a                     ->
                        UUID                                ->
                        s                                   -> IO ()
post h@FSMHandle{..} i s0 =
    fsmCreate fsmStore (mkInstance i s0 [] :: Instance s e a)


-- |Concurrent updates will be serialised by Postgres.
-- Do not call this function for FSM Instances that do not exist yet.
-- Return True when the state transition has been successfully computed
-- and actions have been scheduled.
-- Returns False on failure to compute state transition.
patch :: forall s e a . (FromJSON s, FromJSON e, FromJSON a,
                         ToJSON   s, ToJSON   e, ToJSON   a,
                         Typeable s, Typeable e, Typeable a,
                         Eq       s, Eq       e, Eq       a) =>

                         FSMHandle s e a                     ->
                         UUID                                ->
                         [Msg e]                             -> IO Bool
patch h@FSMHandle{..} i es = do
    openTxn walStore i

    success <- fsmUpdate fsmStore i (patchPhase1 fsmTable es)

    if success
    then recover h i >> return True
    else return False

recover :: forall s e a . (FromJSON s, FromJSON e, FromJSON a,
                           ToJSON   s, ToJSON   e, ToJSON   a,
                           Typeable s, Typeable e, Typeable a,
                           Eq       s, Eq       e, Eq       a) =>
                           FSMHandle s e a                     ->
                           UUID                                -> IO ()
recover h@FSMHandle{..} i =
    void $ forkFinally (fsmUpdate fsmStore i (patchPhase2 fsmTable))
                       (\case Left exn -> do
                                putStrLn $ "Exception occurred while trying to recover " ++ show i
                                print exn
                              Right _  -> closeTxn walStore i)

-- |FIXME: Do something about dead txs.
-- Dead txs are those that do not have a corresponding entry in the main data table.
-- This can happen if there is a problem between writing the WAL entry and the FSM.
recoverAll :: forall s e a . (Eq s, Eq e, Eq a) =>
              FSMHandle s e a -> IO ()
recoverAll h@FSMHandle{..} = do
    wals <- walScan walStore walExpiration
    mapM_ (recover h . walId) wals


upsert :: forall s e a . (FromJSON s, FromJSON e, FromJSON a,
                          ToJSON   s, ToJSON   e, ToJSON   a,
                          Typeable s, Typeable e, Typeable a,
                          Eq s, Eq e, Eq a)                   =>

                          FSMHandle s e a                     ->
                          UUID                                ->
                          s                                   ->
                          [Msg e]                             -> IO ()
upsert h i s es = do
    ms <- get h i
    maybe (post h i s >> void (patch h i es))
          (\s -> void $ patch h i es)
          ms
