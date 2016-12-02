{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}


module FSMApi where

import FSM
import FSMEngine
import FSMStore
import FSMTable
import MemoryStore
import WAL
import WALStore

import Control.Concurrent
import Control.Monad (liftM,forM,void)
import Data.Aeson
import Data.Foldable (forM_)
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time
import Data.Typeable hiding (Proxy)
import GHC.Generics
import System.IO
import System.Timeout

-- |API for FSMs.
-- This is the interface through which you can interact with a FSM
-- from the rest of your program.
data FSMHandle st wal k s e a where
    FSMHandle :: (Eq s, Eq e, Eq a, FSMStore st k s e a, WALStore wal k, FSMKey k) => {
        fsmStore   :: st,
        walStore   :: wal,
        fsmTable   :: FSMTable s e a,
        effTimeout :: Int,
        retryCount :: Int
    } -> FSMHandle st wal k s e a


get :: forall st wal k s e a . FSMStore st k s e a => FSMHandle st wal k s e a -> k -> IO(Maybe s)
get h@FSMHandle{..} k = fsmRead fsmStore k (Proxy :: Proxy k s e a)

-- |Idempotent because of usage of caller-generated UUIDs
-- FIXME: This can throw an exception.
post :: forall st wal k s e a . FSMStore st k s e a =>
        FSMHandle st wal k s e a                                 ->
        k                                                        ->
        s                                                        -> IO ()
post h@FSMHandle{..} k s0 =
    fsmCreate fsmStore (mkInstance k s0 [] :: Instance k s e a)


-- |Concurrent updates will be serialised by Postgres.
-- Do not call this function for FSM Instances that do not exist yet.
-- Return True when the state transition has been successfully computed
-- and actions have been scheduled.
-- Returns False on failure to compute state transition.
patch :: forall st wal k s e a . (FSMStore st k s e a, MealyInstance k s e a, FSMKey k) => FSMHandle st wal k s e a -> k -> [Msg e] -> IO Bool
patch h@FSMHandle{..} k es = do
    openTxn walStore k

    status <- fsmUpdate fsmStore k ((patchPhase1 fsmTable es) :: MachineTransformer s e a)

    if status /= NotFound
    then recover h k >> return True
    else return False


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
          (\s -> void $ patch h k es)
          ms
