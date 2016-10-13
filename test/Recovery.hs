{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Recovery where

import Test.Tasty
import Test.Tasty.HUnit

import CommonDefs

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Typeable
import Data.Aeson
import GHC.Generics

import FSM
import FSMApi
import FSMTable
import PostgresqlStore as Store
import Database.PostgreSQL.Simple as PGS
import Data.Time.Clock
import Data.IORef
import Data.UUID
import Data.UUID.V4
import Debug.Trace

runRecoveryTest c = testCase "Recovery" (runTest c)

data RecoveryState  = RecoveryState  deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)
data RecoveryEvent  = RecoveryEvent  deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)
data RecoveryAction = RecoveryAction deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)

recoveryTransition :: (RecoveryState,RecoveryEvent) -> (RecoveryState,[RecoveryAction])
recoveryTransition (RecoveryState,RecoveryEvent) =
    (RecoveryState,[RecoveryAction])

recoveryEffects :: IORef Bool -> MVar () -> Msg RecoveryAction -> IO Bool
recoveryEffects b sync a = do
    bb <- readIORef b

    -- indicate that we read the IORef and are running the action now
    putMVar sync ()

    return bb


runTest c = do
    st     <- Store.createFsmStore c "RecoveryTest"
    wal    <- Store.createWalStore c "RecoveryTestWal"

    b      <- newIORef False
    sync   <- newEmptyMVar

    let t   = FSMTable recoveryTransition (recoveryEffects b sync)
    let fsm = FSMHandle "RecoveryFSM" t st wal 0    -- hack

    i      <- nextRandom

    post fsm i RecoveryState
    mkMsgs [RecoveryEvent] >>= patch fsm i

    -- action is run for the first time
    readMVar sync

    writeIORef b True
    recoverAll fsm

    -- action is run again
    readMVar sync

    assert True
