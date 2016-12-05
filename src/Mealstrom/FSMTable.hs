{-|
Module      : FSMTable
Description : Types for Transitions and Effects
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org
-}

module Mealstrom.FSMTable where

import Mealstrom.FSM

type Transitions s e a = (s,e) -> (s,[a])

-- |Effects are wrapped in Msgs so that the effects function
-- can access the msgId. This is useful when the effects function
-- sends messages of its own, because it can reuse the msgId, thereby
-- creating a message chain with the same Id. Doing so extends guarantees
-- to the receiving FSM.
type Effects a         = Msg a -> IO Bool

data FSMTable s e a    = FSMTable {
    transitions :: Transitions s e a,
    effects     :: Effects a
}
