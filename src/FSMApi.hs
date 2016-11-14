{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}


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
import Data.Text
import Data.Time
import Data.Typeable
import Data.UUID
import GHC.Generics
import System.IO
import System.Timeout


-- |API for FSMs.
-- This is the interface through which you can interact with a FSM
-- from the rest of your program.
data FSMHandle st wal s e a where
    FSMHandle :: (FSMStore st (Instance s e a), WALStore wal, Eq s, Eq e, Eq a) => {
        fsmName       :: Text,
        fsmTable      :: FSMTable s e a,
        fsmStore      :: st,
        walStore      :: wal,
        effTimeout    :: Int,
        retryCount    :: Int
    } -> FSMHandle st wal s e a


get :: forall st wal s e a . FSMStore st (Instance s e a) =>
       FSMHandle st wal s e a -> UUID                     -> IO(Maybe s)
get h@FSMHandle{..} i = fmap (fmap (evalState fsmTable . machine)) res
  where
      res = fsmRead fsmStore i :: IO (Maybe (Instance s e a))


-- |Idempotent because of usage of caller-generated UUIDs
-- FIXME: This can throw an exception.
post :: forall st wal s e a . FSMStore st (Instance s e a) =>
        FSMHandle st wal s e a                             ->
        UUID                                               ->
        s                                                  -> IO ()
post h@FSMHandle{..} i s0 =
    fsmCreate fsmStore (mkInstance i s0 [] :: Instance s e a)


-- |Concurrent updates will be serialised by Postgres.
-- Do not call this function for FSM Instances that do not exist yet.
-- Return True when the state transition has been successfully computed
-- and actions have been scheduled.
-- Returns False on failure to compute state transition.
patch :: forall st wal s e a . FSMHandle st wal s e a -> UUID -> [Msg e] -> IO Bool
patch h@FSMHandle{..} i es = do
    openTxn walStore i

    status <- fsmUpdate fsmStore i phase1Wrapper -- (patchPhase1 fsmTable es)

    if status /= NotFound
    then recover h i >> return True
    else return False

  where
      phase1Wrapper x = do
        let k = uuid x
        let m = machine x
        nm <- patchPhase1 fsmTable es m
        return $ Instance k nm


recover :: forall st wal s e a . FSMHandle st wal s e a -> UUID -> IO ()
recover h@FSMHandle{..} i
    | retryCount == 0 = hPutStrLn stderr $ "Alarma! Recovery retries for " ++ show i ++ " exhausted. Giving up!"
    | otherwise =
        void $ forkFinally (timeout (effTimeout*10^6) (fsmUpdate fsmStore i phase2Wrapper)) -- (patchPhase2 fsmTable))
                           (\case Left exn      -> do       -- the damn thing crashed, print log and try again
                                      hPutStrLn stderr $ "Exception occurred while trying to recover " ++ show i
                                      hPrint stderr exn
                                      recover h{retryCount = retryCount - 1} i
                                  Right Nothing -> do       -- We hit the timeout. Try again until we hit the retry limit.
                                      hPutStrLn stderr $ "Timeout while trying to recover " ++ show i
                                      recover h{retryCount = retryCount - 1} i
                                  Right (Just Done)    -> closeTxn walStore i    -- All good.
                                  Right (Just Pending) ->                        -- Some actions did not complete successfully.
                                      recover h{retryCount = retryCount - 1} i)
    where
        phase2Wrapper x = do
            let k = uuid x
            let m = machine x
            nm <- patchPhase2 fsmTable m
            return $ Instance k nm


recoverAll :: forall st wal s e a . FSMHandle st wal s e a -> IO ()
recoverAll h@FSMHandle{..} = do
    wals <- walScan walStore effTimeout
    mapM_ (recover h . walId) wals


-- |A helper that is sometimes useful
upsert :: forall st wal s e a . FSMStore st (Instance s e a) =>
                                FSMHandle st wal s e a -> UUID -> s -> [Msg e] -> IO ()
upsert h i s es = do
    ms <- get h i
    maybe (post h i s >> void (patch h i es))
          (\s -> void $ patch h i es)
          ms
