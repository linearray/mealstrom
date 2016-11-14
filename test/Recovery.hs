{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}


module Recovery(runRecoveryTests) where

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
import FSMStore
import FSMTable
import PostgresJSONStore as PGJSON
import MemoryStore       as MemStore
import Data.Text
import Data.Time.Clock
import Data.IORef
import Data.UUID
import Data.UUID.V4
import Debug.Trace

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


runRecoveryTests c = testGroup "Recovery" [
    testCase "RecoveryPG" (runTest $ PGJSON.mkStore c),
    testCase "RecoveryMem" (runTest (MemStore.mkStore :: Text -> IO (MemoryStore RecoveryState RecoveryEvent RecoveryAction)))
    ]

runTest c = do
    st     <- c "RecoveryTest"

    b      <- newIORef False
    sync   <- newEmptyMVar

    let t   = FSMTable recoveryTransition (recoveryEffects b sync)
    let fsm = FSMHandle "RecoveryFSM" t st st 1 3    -- we have a timeout of 1 second for actions

    i      <- nextRandom

    post fsm i RecoveryState
    mkMsgs [RecoveryEvent] >>= patch fsm i

    -- action is run for the first time
    takeMVar sync

    -- wait two seconds, so that the action is definitely recoverable
    threadDelay (2 * 10^6)
    writeIORef b True
    recoverAll fsm

    -- action is run again
    takeMVar sync

    assert True
