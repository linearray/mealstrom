{-# LANGUAGE DataKinds #-}

module FSMEngine(patchPhase1,patchPhase2,evalState) where

-- |This module's purpose is to apply changes to the machine
-- and run effects.

import FSM
import FSMTable

import Control.Monad (filterM, liftM, (>=>), void)

-- |patchPhase1 is the part of a "change" to an FSM that happens synchronously.
patchPhase1 :: (Eq s, Eq e, Eq a) => FSMTable s e a -> [Msg e] -> Machine s e a -> IO (Machine s e a)
patchPhase1 t es m = eval t (sendMultiple m es)

-- |patchPhase2 is the part of a "change" to an FSM that happens *asynchronously*.
patchPhase2 :: (Eq s, Eq e, Eq a) =>  FSMTable s e a -> Machine s e a -> IO (Machine s e a)
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

evalState :: (Eq s, Eq e, Eq a) => FSMTable s e a -> Machine s e a -> s
evalState t m =
    let
        ibox         = inbox m
        obox         = outbox m
        (ids,events) = foldr (\m@(Msg i e) (is,es) -> (i:is,e:es)) ([],[]) ibox
        (newm,_)     = closure (transitions t) m events
    in
        currState newm

-- |Calculate the state changes in response to a message
eval :: (Eq s, Eq e, Eq a) => FSMTable s e a -> Machine s e a -> IO (Machine s e a)
eval t m =
    let
        ibox         = inbox m
        obox         = outbox m
        comm         = committed m
        (ids,events) = foldr (\m@(Msg (Just i) e) (is,es) -> (i:is,e:es)) ([],[]) ibox
        (newm,as)    = closure (transitions t) m events
        asmsgs       = map mkMsg as
    in do
        s <- sequence asmsgs
        return $ newm {inbox = [], outbox = obox ++ s, committed = comm ++ ids}

-- |Take messages from outbox and apply the effects.
-- Failed applications of effects shall remain in the outbox.
apply :: (Eq s, Eq e) => FSMTable s e a -> Machine s e a -> IO (Machine s e a)
apply t m =
    let
        obox = outbox m
        eff  = effects t
    in do
        newas <- filterM (liftM not . eff) obox

        return $ m {outbox = newas}
