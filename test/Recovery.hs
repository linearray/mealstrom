{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Recovery
Description : Test that recovery works
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org
-}
module Recovery(runRecoveryTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Data.Typeable
import Data.Aeson
import GHC.Generics
import Data.Text
import Data.IORef
import Data.UUID
import Data.UUID.V4

import Mealstrom.FSM
import Mealstrom.FSMApi
import Mealstrom.FSMTable
import Mealstrom.PostgresJSONStore as PGJSON
import Mealstrom.MemoryStore       as MemStore

type RecoveryKey    = UUID
data RecoveryState  = RecoveryState1  | RecoveryState2  deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)
data RecoveryEvent  = RecoveryEvent1  | RecoveryEvent2  deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)
data RecoveryAction = RecoveryAction1 | RecoveryAction2 deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)

instance MealyInstance RecoveryKey RecoveryState RecoveryEvent RecoveryAction

recoveryTransition :: (RecoveryState,RecoveryEvent) -> (RecoveryState,[RecoveryAction])
recoveryTransition (RecoveryState1,RecoveryEvent1) =
    (RecoveryState2,[RecoveryAction1])

recoveryEffects :: IORef Bool -> MVar () -> Msg RecoveryAction -> IO Bool
recoveryEffects b sync _a = do
    bb <- readIORef b

    -- indicate that we read the IORef and are running the action now
    putMVar sync ()

    return bb

runRecoveryTests :: String -> TestTree
runRecoveryTests c = testGroup "Recovery" [
    testCase "RecoveryPG" (runTest $ PGJSON.mkStore c),
    testCase "RecoveryMem" (runTest (MemStore.mkStore :: Text -> IO (MemoryStore RecoveryKey RecoveryState RecoveryEvent RecoveryAction)))
    ]

runTest c = do
    st     <- c "RecoveryTest"

    b      <- newIORef False
    sync   <- newEmptyMVar

    let t   = FSMTable recoveryTransition (recoveryEffects b sync)
    let fsm = FSMHandle st st t 1 3    -- we have a timeout of 1 second for actions

    i <- nextRandom

    post fsm i RecoveryState1
    mkMsgs [RecoveryEvent1] >>= patch fsm i

    -- action is run for the first time
    takeMVar sync

    -- wait two seconds, so that the action is definitely recoverable
    threadDelay (2 * 10^6)
    writeIORef b True
    recoverAll fsm

    -- action is run again
    takeMVar sync

    -- If we reach this, then the recovery definitely ran, yet the entry in the DB
    -- might still be wrong. That's ok.
    assert True
