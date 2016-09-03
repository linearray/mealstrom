{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module BasicFSM (runBasicTest)

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Data.Typeable
import Data.Aeson
import GHC.Generics

import FSM
import FSMApi
import FSMTable
import PostgresqlStore as Store
import Database.PostgreSQL.Simple as PGS
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4
import Debug.Trace

runBasicTest = testCase "Basic" runTest

-- Define some stuff
data ConnectionState  = New | Open | Closed
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

data ConnectionEvent  = Create | Close | Reset
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

data ConnectionAction = PrintStatusOpened | PrintStatusClosed
    deriving (Eq,Typeable,Generic,ToJSON,FromJSON)

evalEffects :: Msg ConnectionAction -> IO Bool
evalEffects (Msg i c)
    | c == PrintStatusOpened = putStrLn "OUTPUT: Connection opened" >> return True
    | c == PrintStatusClosed = putStrLn "OUTPUT: Connection closed" >> return True

connTransition :: (ConnectionState, ConnectionEvent) -> (ConnectionState, [ConnectionAction])
connTransition (s,e) =
    case (s,e) of
        (New, Create) -> (Open,  [PrintStatusOpened])
        (Open, Close) -> (Closed,[PrintStatusClosed])
        (Open, Reset) -> (Open,  [PrintStatusClosed, PrintStatusOpened])
        (a, b       ) -> trace ("a is: " ++ show a ++ " b is: " ++ show b) (Open,[])


runTest = do
    let connStr = "host='localhost' port=5432 dbname='fsm' user='amx' password=''"
    st       <- Store.createFsmStore connStr "FSMTest"
    wal      <- Store.createWalStore connStr "FSMTestWal"
    let t     = FSMTable connTransition evalEffects
    let myFSM = FSMHandle "FSMTest" t st wal 900
    firstId  <- nextRandom

    post myFSM firstId New []
    Just fsmState <- get myFSM firstId
    assert $ fsmState == New

    msgs <- mkMsgs [Create]
    patch myFSM firstId msgs

    time <- getCurrentTime
    assert $ busyWaitForState myFSM firstId Open time

    Just fsmState <- get myFSM firstId
    putStrLn $ "State is: " ++ show fsmState
    assert True

  where
    cutOff = 2

    busyWaitForState :: FSMHandle ConnectionState ConnectionEvent ConnectionAction
                     -> UUID
                     -> ConnectionState
                     -> UTCTime
                     -> IO Bool
    busyWaitForState fsm i s t = do
        ct <- getCurrentTime

        if addUTCTime cutOff t < ct
        then return False
        else do
            Just cs <- get fsm i
            if cs == s
            then return True
            else busyWaitForState fsm i s t
