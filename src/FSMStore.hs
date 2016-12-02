{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FSMStore where

import Data.UUID
import FSM

data Proxy k s e a = Proxy

-- |Even the Read class needs type parameters 'e' and 'a' because it needs to deserialise the entry from storage.
class FSMStore st k s e a where
    fsmRead   :: st -> k -> Proxy k s e a -> IO (Maybe s)
    fsmCreate :: st -> Instance k s e a -> IO ()
    fsmUpdate :: st -> k -> Proxy k s e a -> MachineTransformer s e a -> IO OutboxStatus
