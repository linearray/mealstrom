{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module FSMTable where

import Data.List (foldl')
import FSM

type Transitions s e a = (s,e) -> (s,[a])

-- |Effects are wrapped in Msgs so that the effects function
-- can access the msgId. This can be useful when the effects function
-- sends messages of its own.
type Effects a         = Msg a -> IO Bool

data FSMTable s e a    = FSMTable {
    transitions :: Transitions s e a,
    effects     :: Effects a
}

-- |Apply a list of events to a Memory according to a transition function
closure :: (Eq s, Eq e) => Transitions s e a -> Machine s e a -> [e] -> (Machine s e a, [a])
closure trans m@Machine{..} =
    foldl' (\(mym,oldas) e ->
        let (newm, newas) = transition trans m e in
            (newm, oldas ++ newas)
    ) (m,[])

-- |Calculates a new Memory, according to the transition function, for one event.
transition :: (Eq s, Eq e) => Transitions s e a -> Machine s e a -> e -> (Machine s e a, [a])
transition trans Machine{..} e =
    let
        (newState,as) = trans (currState,e)
        newHist       = _append (Step 0 currState e newState as) hist
    in
      (Machine inbox outbox committed initState newState newHist, as)
