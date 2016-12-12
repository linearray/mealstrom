{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : Upgrade
Description : Test that upgrades of the data model actually work
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org

All this looks a bit goofy because you cannot have duplicate
data constructors and I did not want to make separate modules.
In practice it's much less confusing.
-}

module Upgrade (runUpgradeTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Control.Monad               (guard)
import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.Maybe                  (isNothing)
import qualified Data.Text         as Text
import Data.Text                   (Text)
import Data.Typeable               (Typeable)
import Data.UUID
import Data.UUID.V4
import GHC.Generics

import Mealstrom
import Mealstrom.FSMStore
import Mealstrom.WALStore
import Mealstrom.PostgresJSONStore as PGJSON

-- |First FSM Instance
type FirstKey    = UUID
data FirstState  = OldState1 deriving (Eq,Show,Typeable)
instance ToJSON FirstState where
    toJSON s = "OldState1"
instance FromJSON FirstState where
    parseJSON = withText "expected OldState1" (\t -> case t of "OldState1" -> return OldState1)

data FirstEvent  = OldEvent1 deriving (Eq,Show,Typeable)
instance ToJSON FirstEvent where
    toJSON e = "OldEvent1"
instance FromJSON FirstEvent where
    parseJSON = withText "expected OldEvent1" (\t -> case t of "OldEvent1" -> return OldEvent1)

data FirstAction = OldAction1 {
    _a :: String,
    _b :: Int
} deriving (Eq,Show,Typeable)
instance ToJSON FirstAction where
    toJSON a = object [
           "a" .= _a a,
           "b" .= _b a
           ]
instance FromJSON FirstAction where
    parseJSON = withObject "expected OldAction1" $ \o -> do
        _a <- o .: "a"
        _b <- o .: "b"

        return $ OldAction1 _a _b


instance MealyInstance FirstKey FirstState FirstEvent FirstAction
firstTransition :: (FirstState,FirstEvent) -> (FirstState,[FirstAction])
firstTransition (OldState1,OldEvent1) =
    (OldState1,[OldAction1 "lol" 42])

firstEffects :: Msg FirstAction -> IO Bool
firstEffects _a = do
    putStrLn "Action completed"
    return True

-- |Second FSM Instance
-- here we want to parse both values from the first Instance
-- as well as values from the second Instance
type SecondKey    = UUID

-- The first upgrade is adding another data constructor to the State
data SecondState  = NewState1 | NewState2 Int deriving (Eq,Show)
instance ToJSON SecondState where
    toJSON NewState1     = "NewState1"
    toJSON (NewState2 i) = object [
        "name"  .= String "NewState2",
        "value" .= i
        ]
instance FromJSON SecondState where
    parseJSON (Object o) = do
        name <- o .: "name"
        guard (name == String "NewState2")
        val  <- o .: "value"

        return $ NewState2 val

    -- |Conversion from old version
    parseJSON (String t) =
        case t of
            "NewState1" -> return NewState1
            "OldState1" -> return NewState1

-- The second upgrade is adding a parameter to the Event
data SecondEvent = NewEvent1 Int deriving (Eq,Show)
instance ToJSON SecondEvent where
    toJSON (NewEvent1 i) = object [
        "name"  .= String "NewEvent1",
        "value" .= i
        ]
instance FromJSON SecondEvent where
    parseJSON (Object o) = do
        name <- o .: "name"
        guard(name == String "NewEvent1")
        val  <- o .: "value"

        return $ NewEvent1 val

    -- Conversion from old version
    parseJSON (String t) = case t of
        "OldEvent1" -> return $ NewEvent1 0

-- |Here we just add another field to the existing data type
data SecondAction = NewAction1 {
    __a :: String,
    __b :: Int,
    __c :: Double
} deriving (Eq,Show,Typeable)
instance ToJSON SecondAction where
    toJSON a = object [
           "a" .= __a a,
           "b" .= __b a,
           "c" .= __c a
           ]
instance FromJSON SecondAction where
    parseJSON = withObject "expected FirstAction" $ \o -> do
        __a <- o .:  "a"
        __b <- o .:  "b"
        __c <- o .:? "c" .!= 0.0

        return $ NewAction1 __a __b __c


instance MealyInstance SecondKey SecondState SecondEvent SecondAction
secondTransition :: (SecondState,SecondEvent) -> (SecondState,[SecondAction])
secondTransition (NewState1,NewEvent1 i) =
    (NewState2 i,[NewAction1 "double lol" 666 1.0])

secondEffects :: Msg SecondAction -> IO Bool
secondEffects _a = do
    putStrLn "Action completed"
    return True

runUpgradeTests :: String -> TestTree
runUpgradeTests c = testGroup "UpgradeFSM" [
    testCase "UpgradePG" (runTest (PGJSON.mkStore c))
    ]

runTest :: (Text -> IO PostgresJSONStore) -> IO ()
runTest c = do
    -- Start by creating the first FSMHandle and adding some data
    st       <- c "UpgradeTest" :: IO PostgresJSONStore
    let t1     = FSMTable firstTransition firstEffects
    let myFSM1 = FSMHandle st st t1 90 3

    firstId   <- nextRandom
    res1      <- post myFSM1 firstId OldState1
    mkMsgs [OldEvent1] >>= patch myFSM1 firstId

    sndId     <- nextRandom
    res2      <- post myFSM1 sndId OldState1
    mkMsgs [OldEvent1] >>= patch myFSM1 sndId

    -- Then, try to access the same FSM using another MealyInstance that uses upgraded data types
    let t2     = FSMTable secondTransition secondEffects
    let myFSM2 = FSMHandle st st t2 90 3

    -- Conversion when accessing
    res3      <- get myFSM2 firstId
    assert $ res3 == Just NewState1

    -- Conversion when saving
    mkMsgs [NewEvent1 15] >>= patch myFSM2 firstId
    res4      <- get myFSM2 firstId
    assert $ res4 == Just (NewState2 15)

    -- Batch conversion. Also examines already converted instances, but writes them back untouched.
    PGJSON._batchConversion st "UpgradeTest" (Proxy :: Proxy SecondKey SecondState SecondEvent SecondAction)
    res5 <- get myFSM2 sndId
    assert $ res5 == Just NewState1
