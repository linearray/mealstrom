{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : Exception
Description : Test that illegal actions throw an exception and that they are caught.
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org
-}

module Exception (runExceptionTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Data.Aeson
import Data.Hashable
import Data.Maybe                  (isNothing)
import Data.Text                   as Text
import Data.Typeable
import Data.UUID
import Data.UUID.V4
import GHC.Generics

import Mealstrom
import Mealstrom.FSMStore
import Mealstrom.WALStore
import Mealstrom.PostgresJSONStore as PGJSON
import Mealstrom.MemoryStore       as MemStore


type MyKey    = UUID
data MyState  = MyState1  | MyState2  deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)
data MyEvent  = MyEvent1  | MyEvent2  deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)
data MyAction = MyAction1 | MyAction2 deriving (Eq,Show,Generic,Typeable,ToJSON,FromJSON)

instance MealyInstance MyKey MyState MyEvent MyAction
myTransition :: (MyState,MyEvent) -> (MyState,[MyAction])
myTransition (MyState1,MyEvent1) =
    (MyState2,[MyAction1])

myEffects :: Msg MyAction -> IO Bool
myEffects _a = do
    putStrLn "Action completed"
    return True


runExceptionTests :: String -> TestTree
runExceptionTests c = testGroup "ExceptionFSM" [
    testCase "ExceptionPG" (runTest (PGJSON.mkStore c)),
    testCase "ExceptionMem" (runTest (MemStore.mkStore :: Text -> IO (MemoryStore MyKey MyState MyEvent MyAction)))
    ]

runTest :: (FSMStore st MyKey MyState MyEvent MyAction,
            WALStore st MyKey) => (Text -> IO st) -> IO ()
runTest c = do
    st       <- c "ExceptionTest"
    sync     <- newEmptyMVar
    let t     = FSMTable myTransition myEffects
    let myFSM = FSMHandle st st t 90 3
    firstId <- nextRandom

    res1 <- post myFSM firstId MyState1
    res2 <- post myFSM firstId MyState1

    assert res1
    assert $ not res2

    -- Technically the following assertions are not concerned with exceptions, but fine, whatever
    secondId <- nextRandom

    res3 <- get myFSM secondId
    assert $ isNothing res3

    res4 <- mkMsgs [MyEvent1] >>= patch myFSM secondId
    assert $ not res4
