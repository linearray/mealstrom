{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |This module's purpose is to apply changes to the machine
-- and run effects.
module FSMEngine(patchPhase1,patchPhase2) where

import FSM
import FSMTable

import Control.Monad (filterM, liftM, (>=>), void)
import Data.List

-- |patchPhase1 is the part of a "change" to an FSM that happens synchronously.
patchPhase1 :: (Eq s, Eq e) => FSMTable s e a -> [Msg e] -> Machine s e a -> IO (Machine s e a)
patchPhase1 tab es m = eval tab (sendMultiple m es)

-- |patchPhase2 is the part of a "change" to an FSM that happens *asynchronously*.
patchPhase2 :: (Eq a) => FSMTable s e a -> Machine s e a -> IO (Machine s e a)
patchPhase2 = apply


-- |Wrapper to send multiple messages at once.
sendMultiple :: Machine s e a -> [Msg e] -> Machine s e a
sendMultiple = foldr (flip send)

-- |See if the message has already been recorded once
-- If not, add it to the inbox.
send :: Machine s e a -> Msg e -> Machine s e a
send m e =
    let
        msgId (Msg (Just i) _) = i
        ibox = inbox m
    in
        if elem (msgId e) $ map msgId ibox ++ committed m
        then m
        else m {inbox = ibox ++ [e]}

-- |Calculate the state changes in response to a message
eval :: (Eq s, Eq e) => FSMTable s e a -> Machine s e a -> IO (Machine s e a)
eval FSMTable{..} m =
    let
        ibox         = inbox m
        obox         = outbox m
        comm         = committed m
        (ids,events) = foldr (\m@(Msg (Just i) e) (is,es) -> (i:is,e:es)) ([],[]) ibox
        (newm,as)    = closure transitions m events
        asmsgs       = map mkMsg as
    in do
        s <- sequence asmsgs
        return $ newm {inbox = [], outbox = obox ++ s, committed = comm ++ ids}

-- |Take messages from outbox and apply the effects.
-- Failed applications of effects shall remain in the outbox.
apply :: (Eq a) => FSMTable s e a -> Machine s e a -> IO (Machine s e a)
apply FSMTable{..} m = do
    newas <- filterM (liftM not . effects) (outbox m)

    return $ m {outbox = newas}

-- |Apply a list of events to a Memory according to a transition function
closure :: (Eq s, Eq e) => Transitions s e a -> Machine s e a -> [e] -> (Machine s e a, [a])
closure trans m@Machine{..} =
    foldl' (\(mym,oldas) e ->
        let (newm, newas) = step trans m e in
            (newm, oldas ++ newas)
    ) (m,[])

-- |Calculates a new Memory, according to the transition function, for one event.
step :: (Eq s, Eq e) => Transitions s e a -> Machine s e a -> e -> (Machine s e a, [a])
step trans Machine{..} e =
    let
        (newState,as) = trans (currState,e)
        newHist       = _append (Step 0 currState e newState as) hist
    in
      (Machine inbox outbox committed initState newState newHist, as)
