{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : WALStore
Description : Store WALEntries
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org

A WALStore is anything being able to store WALEntries.
WALEntries indicate how often a recovery process has been started for
an instance.
-}
module WALStore where

import Data.Time.Clock
import Data.UUID

import FSM

class WALStore st k where
    walUpsertIncrement :: st -> k -> IO ()
    walDecrement       :: st -> k -> IO ()
    walScan            :: st -> Int  -> IO [WALEntry k]

data WALEntry k = WALEntry {
    walId    :: k,
    walTime  :: UTCTime,
    walCount :: Int
} deriving (Show,Eq)

openTxn :: WALStore st k => st -> k -> IO ()
openTxn = walUpsertIncrement

closeTxn :: WALStore st k => st -> k -> IO ()
closeTxn = walDecrement
