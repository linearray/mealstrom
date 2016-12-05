{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : CounterFSM
Description : Show how to "compress" multiple events into one.
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org

After this test has run the DB table should show a "Count" entry
instead of ten individual Desu events.
-}

module CounterFSM (runCounterTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Typeable
import GHC.Generics
import Data.UUID
import Data.UUID.V4

import Mealstrom.FSM
import Mealstrom.FSMApi
import Mealstrom.FSMTable
import Mealstrom.PostgresJSONStore as PGJSON
import Mealstrom.MemoryStore       as MemStore

type CounterKey   = UUID
data CounterState = Desu
    deriving (Eq,Show,Typeable)
instance ToJSON CounterState where
    toJSON _ = "Desu"
instance FromJSON CounterState where
    parseJSON "Desu" = return Desu

data CounterEvent = DesuEvent
    deriving (Eq,Show,Typeable)
instance ToJSON CounterEvent where
    toJSON _ = "DesuEvent"
instance FromJSON CounterEvent where
    parseJSON "DesuEvent" = return DesuEvent


-- NOP
data CounterAction = DesuDummyAction
    deriving (Eq,Show,Typeable,Generic)
instance ToJSON CounterAction where
    toJSON _ = "DesuDummyAction"
instance FromJSON CounterAction where
    parseJSON "DesuDummyAction" = return DesuDummyAction


instance MealyInstance CounterKey CounterState CounterEvent CounterAction

counterTransition :: (CounterState, CounterEvent) -> (CounterState,[CounterAction])
counterTransition =
    \case (Desu, DesuEvent) -> (Desu,[DesuDummyAction])

counterEffects :: MVar () -> Msg CounterAction -> IO Bool
counterEffects mvar _ =
    putMVar mvar () >> return True

runCounterTests :: String -> TestTree
runCounterTests c = testGroup "CounterFSM" [
    testCase "CounterPG" (runTest $ PGJSON.mkStore c),
    testCase "CounterMem" (runTest (MemStore.mkStore :: Text -> IO (MemoryStore CounterKey CounterState CounterEvent CounterAction)))
    ]

runTest c = do
    sync   <- newEmptyMVar

    st     <- c "CounterTest"

    let t   = FSMTable counterTransition (counterEffects sync)
    let fsm = FSMHandle st st t 900 3

    i      <- nextRandom
    post fsm i Desu

    replicateM_ 10 (do
        m <- mkMsgs [DesuEvent]
        _ <- patch fsm i m
        takeMVar sync)

    s <- get fsm i

    assert $ s == Just Desu
