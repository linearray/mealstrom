{-|
Module      : CommonDefs
Description : Some things that sometimes come in handy.
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org
-}
module CommonDefs where

import Data.Aeson
import Data.Time
import Data.Typeable
import Data.UUID
import Debug.Trace
import GHC.Generics
import FSM
import FSMApi
import FSMStore


cutOff = 2

-- |Don't ever use this in production :^)
busyWaitForState :: (FromJSON s, FromJSON e, FromJSON a,
                     Typeable s, Typeable e, Typeable a,
                     Eq s, Eq e, Eq a, MealyInstance k s e a, FSMStore st k s e a)
                 => FSMHandle st wal k s e a
                 -> k
                 -> s
                 -> UTCTime
                 -> IO Bool
busyWaitForState fsm i s t = do
    ct <- getCurrentTime

    if addUTCTime cutOff t < ct
    then return False
    else do
        mcs <- get fsm i

        if mcs == Just s
        then return True
        else busyWaitForState fsm i s t
