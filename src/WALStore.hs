
{-|
Module      : WALStore
Description : Store WALEntries
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org
Stability   : experimental

A WALStore is anything being able to store WALEntries.
WALEntries indicate how often a recovery process has been started for
an instance.
-}
module WALStore where

import Data.Time.Clock
import Data.UUID

import FSM

class WALStore st where
    walUpsertIncrement :: st -> UUID -> IO ()
    walDecrement       :: st -> UUID -> IO ()
    walScan            :: st -> Int  -> IO [WALEntry]

data WALEntry = WALEntry {
    walId    :: UUID,
    walTime  :: UTCTime,
    walCount :: Int
} deriving (Show,Eq)
