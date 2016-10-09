module CommonDefs where

import Data.Aeson
import Data.Time
import Data.Typeable
import Data.UUID
import Debug.Trace
import GHC.Generics
import FSM
import FSMApi


cutOff = 2

-- don't ever use this in production :^)
busyWaitForState :: (FromJSON s, FromJSON e, FromJSON a,
                     Typeable s, Typeable e, Typeable a,
                     Eq s, Eq e, Eq a)
                 => FSMHandle s e a
                 -> UUID
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
