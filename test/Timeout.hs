{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}



module Timeout(runTimeoutTests) where

import Test.Tasty
import Test.Tasty.HUnit

--import CommonDefs

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

data TimeoutState  = TimeoutState  deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)
data TimeoutEvent  = TimeoutEvent  deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)
data TimeoutAction = TimeoutAction deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)

timeoutTransition :: (TimeoutState,TimeoutEvent) -> (TimeoutState,[TimeoutAction])
timeoutTransition (TimeoutState,TimeoutEvent) = (TimeoutState,[TimeoutAction])

timeoutEffects :: IORef Bool -> MVar () -> Msg TimeoutAction -> IO Bool
timeoutEffects b sync a = do
    bb <- readIORef b

    if   bb
    then putMVar sync ()               -- if this is the second run, we proceed normally
    else seq (sum [1..]) (return ())   -- else we timeout

    return True


runTimeoutTests c = testGroup "Timeout" [
    testCase "TimeoutPG" (runTest $ PGJSON.mkStore c)
    --testCase "TimeoutMem" (runTest (MemStore.mkStore :: Text -> IO (MemoryStore TimeoutState TimeoutEvent TimeoutAction)))
    ]

runTest c = do
    st     <- c "TimeoutTest"

    b      <- newIORef False
    sync   <- newEmptyMVar

    let t   = FSMTable timeoutTransition (timeoutEffects b sync)
    let fsm = FSMHandle "TimeoutFSM" t st st 1 2    -- timeout of 1 second and we only try once

    i      <- nextRandom

    post fsm i TimeoutState
    mkMsgs [TimeoutEvent] >>= patch fsm i

    -- action is run for the first time and should timeout
    threadDelay (1*10^6)
    writeIORef b True

    -- action is run again
    takeMVar sync

    assert True
