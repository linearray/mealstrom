{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mealstrom.FSMApi
Description : API for FSMs
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org

This is the interface through which you primarily interact with a FSM
from the rest of your program.
-}

module Mealstrom.FSMApi where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad          (void)
import qualified Data.Text           as  Text
import           System.IO
import           System.Timeout

import           Mealstrom.FSM
import           Mealstrom.FSMEngine
import           Mealstrom.FSMStore
import           Mealstrom.FSMTable
import           Mealstrom.WALStore


data FSMHandle st wal k s e a where
    FSMHandle :: (Eq s, Eq e, Eq a, FSMStore st k s e a, WALStore wal k, FSMKey k) => {
        fsmStore   :: st,                -- ^ Which backend to use for storing FSMs.
        walStore   :: wal,               -- ^ Which backend to use for the WAL.
        fsmTable   :: FSMTable s e a,    -- ^ A table of transitions and effects.
                                         --   This is not in a typeclass, because you may want to use MVars or similar in effects.
                                         --   See the tests for examples.
        effTimeout :: Int,               -- ^ How much time to allow for Actions until they are considered failed.
        retryCount :: Int                -- ^ How often to automatically retry actions.
    } -> FSMHandle st wal k s e a


get :: forall st wal k s e a . FSMStore st k s e a => FSMHandle st wal k s e a -> k -> IO(Maybe s)
get FSMHandle{..} k = fsmRead fsmStore k (Proxy :: Proxy k s e a)


-- |Idempotent because of usage of caller-generated UUIDs
post :: forall st wal k s e a . FSMStore st k s e a =>
        FSMHandle st wal k s e a                                 ->
        k                                                        ->
        s                                                        -> IO Bool
post FSMHandle{..} k s0 =
    fsmCreate fsmStore (mkInstance k s0 [] :: Instance k s e a) >>= \case
        Nothing -> return True
        Just s  -> hPutStrLn stderr s >> return False


-- |Concurrent updates will be serialised by Postgres.
-- Returns True when the state transition has been successfully computed
-- and actions have been scheduled.
-- Returns False on failure.
patch :: forall st wal k s e a . (FSMStore st k s e a, MealyInstance k s e a, FSMKey k) => FSMHandle st wal k s e a -> k -> [Msg e] -> IO Bool
patch h@FSMHandle{..} k es = do
    openTxn walStore k

    status <- handle (\(e::SomeException) -> hPutStrLn stderr (show e) >> return MealyError)
                     (fsmUpdate fsmStore k ((patchPhase1 fsmTable es) :: MachineTransformer s e a))

    if status /= MealyError
    then recover h k >> return True
    else return False


-- |Recovering is the process of asynchronously applying Actions. It is performed
-- immediately after the synchronous part of an update and, on failure, retried until it
-- succeeds or the retry limit is hit.
recover :: forall st wal k s e a . (FSMStore st k s e a, MealyInstance k s e a, FSMKey k) => FSMHandle st wal k s e a -> k -> IO ()
recover h@FSMHandle{..} k
    | retryCount == 0 = hPutStrLn stderr $ "Alarma! Recovery retries for " ++ Text.unpack (toText k) ++ " exhausted. Giving up!"
    | otherwise =
        void $ forkFinally (timeout (effTimeout*10^6) (fsmUpdate fsmStore k (patchPhase2 fsmTable :: MachineTransformer s e a))) -- (patchPhase2 fsmTable))
                           (\case Left exn      -> do       -- the damn thing crashed, print log and try again
                                      hPutStrLn stderr $ "Exception occurred while trying to recover " ++ Text.unpack (toText k)
                                      hPrint stderr exn
                                      recover h{retryCount = retryCount - 1} k
                                  Right Nothing -> do       -- We hit the timeout. Try again until we hit the retry limit.
                                      hPutStrLn stderr $ "Timeout while trying to recover " ++ Text.unpack (toText k)
                                      recover h{retryCount = retryCount - 1} k
                                  Right (Just Done)    -> closeTxn walStore k    -- All good.
                                  Right (Just Pending) ->                        -- Some actions did not complete successfully.
                                      recover h{retryCount = retryCount - 1} k)


-- |During certain long-lasting failures, like network outage, the retry limit of Actions will be exhausted.
-- You should regularly, e.g. ever 10 minutes, call this function to clean up those hard cases.
recoverAll :: forall st wal k s e a . (MealyInstance k s e a) => FSMHandle st wal k s e a -> IO ()
recoverAll h@FSMHandle{..} = do
    wals <- walScan walStore effTimeout
    mapM_ (recover h . walId) wals


-- |A helper that is sometimes useful
upsert :: forall st wal k s e a . MealyInstance k s e a => FSMStore st k s e a =>
          FSMHandle st wal k s e a -> k -> s -> [Msg e] -> IO ()
upsert h k s es = do
    ms <- get h k
    maybe (post h k s >> void (patch h k es))
          (\_s -> void $ patch h k es)
          ms
