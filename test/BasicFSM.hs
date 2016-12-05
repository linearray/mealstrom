{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : BasicFSM
Description : A simple example.
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org
-}

module BasicFSM (runBasicTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Data.Aeson
import Data.Hashable
import Data.Text                   as Text
import Data.Typeable
import GHC.Generics

import Mealstrom
import Mealstrom.FSMStore
import Mealstrom.WALStore
import Mealstrom.PostgresJSONStore as PGJSON
import Mealstrom.MemoryStore       as MemStore


-- ####################
-- # Connection Example
-- ####################

-- This is a contrived example of how to use a custom Key type, instead of the recommended Text and UUID.
newtype ConnectionKey = ConnectionKey (Int,Int) deriving (Show,Eq,Generic,Hashable)

instance FSMKey ConnectionKey where
    toText (ConnectionKey (a,b)) = Text.pack $ "(" ++ show a ++ "," ++ show b ++ ")"
    fromText t = case fmap (\s -> read (unpack s) :: Int) (splitOn "," $ Text.dropEnd 1 (Text.drop 1 t)) of
        a:[b] -> ConnectionKey (a,b)
        _     -> error ""

data ConnectionState  = New | Open | Closed
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

data ConnectionEvent  = Create | Close | Reset
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

data ConnectionAction = PrintStatusOpened | PrintStatusClosed
    deriving (Eq,Typeable,Generic,ToJSON,FromJSON)

instance MealyInstance ConnectionKey ConnectionState ConnectionEvent ConnectionAction

connEffects :: MVar () -> Msg ConnectionAction -> IO Bool
connEffects mvar (Msg _i c)
    | c == PrintStatusOpened = putStrLn "OUTPUT: Connection opened" >> putMVar mvar () >> return True
    | c == PrintStatusClosed = putStrLn "OUTPUT: Connection closed" >> putMVar mvar () >> return True

connTransition :: (ConnectionState, ConnectionEvent) -> (ConnectionState, [ConnectionAction])
connTransition (s,e) =
    case (s,e) of
        (New, Create) -> (Open,  [PrintStatusOpened])
        (Open, Close) -> (Closed,[PrintStatusClosed])
        (Open, Reset) -> (Open,  [PrintStatusClosed, PrintStatusOpened])

runBasicTests :: String -> TestTree
runBasicTests c = testGroup "BasicFSM" [
    testCase "BasicPG" (runTest (PGJSON.mkStore c)),
    testCase "BasicMem0" (runTest (MemStore.mkStore :: Text -> IO (MemoryStore ConnectionKey ConnectionState ConnectionEvent ConnectionAction)))
    ]

runTest :: (FSMStore st ConnectionKey ConnectionState ConnectionEvent ConnectionAction,
            WALStore st ConnectionKey) => (Text -> IO st) -> IO ()
runTest c = do
    st       <- c "BasicFSMTest"
    sync     <- newEmptyMVar
    let t     = FSMTable connTransition (connEffects sync)
    let myFSM = FSMHandle st st t 90 3
    let firstId = ConnectionKey (1231231,21)   -- This represents a socket or something

    post myFSM firstId New
    Just fsmState1 <- get myFSM firstId
    assert $ fsmState1 == New

    msg1 <- mkMsgs [Create]
    _    <- patch myFSM firstId msg1

    takeMVar sync
    Just fsmState2 <- get myFSM firstId
    assert $ fsmState2 == Open

    msg2 <- mkMsgs [Close]
    _    <- patch myFSM firstId msg2

    takeMVar sync
    Just fsmState3 <- get myFSM firstId
    assert $ fsmState3 == Closed
