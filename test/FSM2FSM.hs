{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : FSM2FSM
Description : An example that shows how two FSMs can exchange messages with each other.
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org
-}

module FSM2FSM (runFSM2FSMTests) where

import Control.Concurrent.QSem
import Data.Aeson
import Data.Text
import Data.Typeable
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Mealstrom
import Mealstrom.FSMStore
import Mealstrom.PostgresJSONStore as PGJSON
import Mealstrom.MemoryStore       as MemStore

-- #################
-- # Payment Example
-- #################

-- #######
-- # FSM 1
-- #######
type PaymentKey   = UUID
data PaymentState = PaymentPending Int | PaymentPaid | PaymentAborted
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

-- Yes, if you abort payment after it has been partially paid, you lose money :-)
data PaymentEvent = ReceivedPayment UUID Int | AbortPayment
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

-- Credit our own bank account with sweet funds
data PaymentAction = PaymentUpdateAccount UUID Int
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

paymentTransition :: (PaymentState, PaymentEvent) -> (PaymentState,[PaymentAction])
paymentTransition (s,e) = case (s,e) of
        (PaymentPending _o, AbortPayment)         -> (PaymentAborted,[])
        (PaymentPending o,  ReceivedPayment ba i) -> if i >= o
                                                     then (PaymentPaid, [PaymentUpdateAccount ba i])
                                                     else (PaymentPending (o-i),[])
        (PaymentAborted,   _)                     -> (PaymentAborted, [])

paymentEffects :: (FSMStore st BankAccountKey BankAccountState BankAccountEvent BankAccountAction)
               =>  QSem
               ->  FSMHandle st wal BankAccountKey BankAccountState BankAccountEvent BankAccountAction
               ->  Msg PaymentAction
               ->  IO Bool
paymentEffects qsem h (Msg d (PaymentUpdateAccount acc amount)) = do

    -- send message to bankaccount FSM using the same msgId!
    upsert h acc (BankAccountBalance 0) [Msg d (BankAccountDeposit amount)]
    signalQSem qsem
    return True

instance MealyInstance PaymentKey PaymentState PaymentEvent PaymentAction

-- #######
-- # FSM 2
-- #######
type BankAccountKey   = UUID
data BankAccountState = BankAccountBalance Int
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

data BankAccountEvent = BankAccountDeposit Int
    deriving (Eq,Show,Typeable,Generic,ToJSON,FromJSON)

-- NOP
data BankAccountAction = BankAccountDummyAction
    deriving (Eq,Show,Typeable,Generic)

instance ToJSON BankAccountAction where
    toJSON _ = "BankAccountDummyAction"

instance FromJSON BankAccountAction where
    parseJSON "BankAccountDummyAction" = return BankAccountDummyAction

bankAccountTransition :: (BankAccountState, BankAccountEvent) -> (BankAccountState,[BankAccountAction])
bankAccountTransition =
    \case (BankAccountBalance i, BankAccountDeposit j) -> (BankAccountBalance $ i + j, [BankAccountDummyAction])

bankAccountEffects :: QSem -> Msg BankAccountAction -> IO Bool
bankAccountEffects qsem _ = signalQSem qsem >> return True

instance MealyInstance BankAccountKey BankAccountState BankAccountEvent BankAccountAction

-- #######
-- # TEST
-- #######
runFSM2FSMTests :: String -> TestTree
runFSM2FSMTests c =
    testGroup "FSM2FSM" [
        testCase "FSM2FSMPG" (runTest (PGJSON.mkStore c)(PGJSON.mkStore c)),
        testCase "FSM2FSMMem" (runTest (MemStore.mkStore :: Text -> IO(MemoryStore BankAccountKey BankAccountState BankAccountEvent BankAccountAction))
                                       (MemStore.mkStore :: Text -> IO(MemoryStore PaymentKey     PaymentState     PaymentEvent     PaymentAction)))
    ]
  where
    runTest c1 c2 = do
        sync          <- newQSem 0

        st1           <- c1 "FSM2FSMTestBank"

        let t1         = FSMTable bankAccountTransition (bankAccountEffects sync)
        let bankFsm    = FSMHandle st1 st1 t1 900 3

        -- Using the first handle we can instantiate the second one.
        st2           <- c2 "FSM2FSMTestPayments"

        let t2         = FSMTable paymentTransition (paymentEffects sync bankFsm)
        let paymentFsm = FSMHandle st2 st2 t2 900 3

        paymentId     <- nextRandom
        bankAccount   <- nextRandom

        msg1          <- mkMsg $ ReceivedPayment bankAccount 1000
        post  paymentFsm paymentId (PaymentPending 1000)
        _ <- patch paymentFsm paymentId [msg1]

        waitQSem sync
        waitQSem sync
        pymtstatus <- get paymentFsm paymentId
        assert $ pymtstatus == Just PaymentPaid

        -- Now check that the second FSM has been updated as well
        bankstatus <- get bankFsm bankAccount
        assert $ bankstatus == Just (BankAccountBalance 1000)
