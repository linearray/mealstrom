{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module BasicFSM (runBasicTests) where

import Test.Tasty
import Test.Tasty.HUnit

import CommonDefs

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Typeable
import Data.Aeson
import GHC.Generics

import FSM
import FSMApi
import FSMStore
import FSMTable
import WALStore
import PostgresJSONStore          as PGJSON
import MemoryStore                as MemStore
import Data.Text
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4
import Debug.Trace

-- ####################
-- # Connection Example
-- ####################
data ConnectionState  = New | Open | Closed
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

data ConnectionEvent  = Create | Close | Reset
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

data ConnectionAction = PrintStatusOpened | PrintStatusClosed
    deriving (Eq,Typeable,Generic,ToJSON,FromJSON)

connEffects :: MVar () -> Msg ConnectionAction -> IO Bool
connEffects mvar (Msg i c)
    | c == PrintStatusOpened = putStrLn "OUTPUT: Connection opened" >> putMVar mvar () >> return True
    | c == PrintStatusClosed = putStrLn "OUTPUT: Connection closed" >> putMVar mvar () >> return True

connTransition :: (ConnectionState, ConnectionEvent) -> (ConnectionState, [ConnectionAction])
connTransition (s,e) =
    case (s,e) of
        (New, Create) -> (Open,  [PrintStatusOpened])
        (Open, Close) -> (Closed,[PrintStatusClosed])
        (Open, Reset) -> (Open,  [PrintStatusClosed, PrintStatusOpened])

runBasicTests c = testGroup "BasicFSM" [
    testCase "BasicPG" (runTest (PGJSON.mkStore c)),
    testCase "BasicMem0" (runTest (MemStore.mkStore :: Text -> IO (MemoryStore ConnectionState ConnectionEvent ConnectionAction)))
    ]

runTest :: (FSMStore st (Instance ConnectionState ConnectionEvent ConnectionAction),
            WALStore st) => (Text -> IO st) -> IO ()
runTest c = do
    st       <- c "BasicFSMTest"
    sync     <- newEmptyMVar
    let t     = FSMTable connTransition (connEffects sync)
    let myFSM = FSMHandle "BasicFSMTest" t st st 90 3
    firstId  <- nextRandom

    post myFSM firstId New
    Just fsmState <- get myFSM firstId
    assert $ fsmState == New

    msg1 <- mkMsgs [Create]
    patch myFSM firstId msg1

    takeMVar sync
    Just fsmState <- get myFSM firstId
    assert $ fsmState == Open

    msg2 <- mkMsgs [Close]
    patch myFSM firstId msg2

    takeMVar sync
    Just fsmState <- get myFSM firstId
    assert $ fsmState == Closed
