{-# LANGUAGE MultiParamTypeClasses #-}

module FSMStore where

import Data.UUID
import FSM

class FSMStore st a where
    fsmRead            :: st -> UUID                   -> IO (Maybe a)
    fsmCreate          :: st -> a                      -> IO ()
    fsmUpdate          :: st -> UUID    -> (a -> IO a) -> IO OutboxStatus

-- FIXME: The fact that (a -> IO a) means we end up with (Instance s e a -> IO (Instance s e a))
-- leaves a bad taste. Right now the stores just ignore the new ID returned and use the old one
-- instead. Hopefully a better solution that doesn't lead to a ton of problems will come up.
