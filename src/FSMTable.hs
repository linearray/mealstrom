{-# LANGUAGE RecordWildCards #-}

module FSMTable where

import Data.List (foldl')
import FSM

--type Transitions s e a = (s,e) -> (s,[a])

-- |Effects are wrapped in Msgs so that the effects function
-- can access the msgId. This can be useful when the effects function
-- sends messages of its own.
--type Effects a         = Msg a -> IO Bool

--data FSMTable s e a    = FSMTable {
--    transitions :: Transitions s e a,
--    effects     :: Effects a
--}

