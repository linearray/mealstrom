Mealstrom
=========

Mealstrom is a way of modeling, storing and running (business) processes using PostgreSQL. It is based on an idea that [Jakob Sievers](http://canned.primat.es/) had when we both worked at a payment service provider.

It is a remedy for a number of drawbacks of using relational database systems directly for the same task, while still building on some of their strengths.

You often want to store not just the current state of a process instantiation, but keep a log of all steps taken so far. Obviously you cannot simply update the previous state in a relational database.

* Therefore in a RDBMS you must store *events* in their own right, and have a way to compute the object's current state. You need to implement checks for what constitutes *valid state transitions* manually, again and again for each entity.

* While RDBMS are very powerful, it often feels like you are doing all the work twice, e.g. you write constraints, foreign key checks, triggers etc. and then do all the input validity checking in the client as well, because you do not want to incur the overhead of constantly sending all input to the DB and because relying on parsing the thrown exceptions is often not even possible.

* If you want to make sure that updates are actually applied, keep in mind that database transactions guarantee all-or-nothing handling of your updates, but you do not necessarily know which one of the two happened! Your database connection can drop between when a transaction completes and when control returns to your session. Hence, you need to make all your updates idempotent, and where they are not naturally, you need to add client-generated IDs to your queries (and perhaps use some of the RDBMS' power like triggers). That assumes you actually read the part on transaction isolation in your database manual, because the details are surprisingly tricky and the tiniest mistake can lead to data loss.

In short: If you are not very careful, modeling state transitions in your processes becomes a tangled mess of SQL queries and code, with duplicated functionality and the potential of race conditions and low assurances of correctness.

##Enter Mealstrom

With Mealstrom you model your process as a finite-state automaton, a Mealy machine to be precise. A Mealy machine, in contrast to a Moore machine, is an FSA that attaches effects to transitions instead of states.

Modeling a process as an FSA is the natural way to do it. FSAs have defined states, defined transitions and rules which transitions are permissable in a given state.

You can then create instances of the machine definition and manipulate them using API functions.

A Mealy machine in Mealstrom has the types **State**, **Event** and **Action**, an instance furthermore has a type **Key**. Mealstrom comes with support for `Text` and `UUID` as the Key type. You can have your own Key type, if you make it an instance of `(FSMKey k)` and implement `toText :: k -> Text` and `fromText :: Text -> k`. If you have no preference, it is recommended to use `UUID`.

To persist the machines to PostgreSQL, you need to have Aeson `ToJSON`, `FromJSON` and `Typeable` instances for your four types. Typically, they can be derived generically.

Once you have your four types, you make an instance of `MealyInstance`.

Let's go through an example - A simple system a surgery ward might use to track patients.

```
-- First the language extension and import dance:

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

import Data.Aeson
import Data.Text (Text)
import Data.Typeable
import GHC.Generics

import Mealstrom
import Mealstrom.PostgresJSONStore as PGJSON

type SSN              = Text
data Limb             = Arm | Hand | Leg deriving (Show,Eq,Generic,ToJSON,FromJSON,Typeable)
data PatientStatus    = PatientAdmitted Integer [LimbSurgery]
                      | PatientReleased
                      | PatientDeceased
                  deriving (Show,Eq,Generic,ToJSON,FromJSON,Typeable)
data LimbSurgery      = Removed Limb | Attached Limb deriving (Show,Eq,Generic,ToJSON,FromJSON,Typeable)
data Event            = Operation LimbSurgery | Release | Deceased deriving (Show,Eq,Generic,ToJSON,FromJSON,Typeable)
data Action           = SendBill Integer | SendCondolences deriving (Show,Eq,Generic,ToJSON,FromJSON,Typeable)

instance MealyInstance SSN PatientStatus Event Action

```

There is also a transition function `transition :: (State,Event) -> (State,[Action])`,

as well as an effects function `effects :: Msg Action -> IO Bool`.

You implement `transition` to indicate which transitions are valid and which effects you want to run when a transition occurs.

An `Action` (wrapped in a Msg) is then used to pattern match in `effects` and execute the appropriate code.

NB *Action* is the type you use to represent the *effects* you want to run.

Because the states, events, actions as well as the transition/effects functions are just Haskell data types and code, you can go crazy, but for now let's expand on the simple example above:

```
-- |Calculates current number of specified limb on patient
-- Boldly assumes every patient comes in with full set of limbs
limbsOnPatient :: [LimbSurgery] -> Limb -> Int
limbsOnPatient ops limb =
    foldr (\op acc -> if
            | op == Removed limb  -> acc-1
            | op == Attached limb -> acc+1
            | otherwise           -> acc) 2 ops

cost :: LimbSurgery -> Integer
cost (Removed Arm)   =  5000
cost (Attached Arm)  = 15000
cost (Removed Hand)  =  2000
cost (Attached Hand) =  8000
cost (Removed Leg)   = 12000
cost (Attached Leg)  = 20000

tr (PatientAdmitted bill ls, Operation (Removed l))
    | limbsOnPatient ls l < 1 = error "Cannot remove limb that's not there anymore!"
    | otherwise               = let newbill = bill + cost (Removed l) in
        (PatientAdmitted newbill $ Removed l : ls, [SendCondolences])

tr (PatientAdmitted bill ls, Operation (Attached l))
    | limbsOnPatient ls l > 1 = error "Cannot attach limb, there is no space!"
    | otherwise               = let newbill = bill + cost (Attached l) in
        (PatientAdmitted newbill $ Attached l : ls, [])

tr (PatientAdmitted bill _ls, Release)  = (PatientReleased, [SendBill bill])
tr (PatientAdmitted bill _ls, Deceased) = (PatientDeceased, [SendCondolences, SendBill bill])

tr (PatientReleased, _) = error "Patient escaped, operation invalid."
tr (PatientDeceased, _) = error "Operations on dead patients are not billable"

eff :: Msg Action -> IO Bool
eff (Msg msgId SendCondolences) = putStrLn "not implemented" >> return True
eff (Msg msgId (SendBill bill)) = charge bill :: IO Bool
```


From wherever you wish to manipulate a Patient instance, you can then use a simple REST-like interface:

```
main = do
    st <- PGJSON.mkStore "host='localhost' port=5432 dbname='butchershop'" "Patient"

    -- You specify transition and effects when creating the Handle for a machine
    -- This is so that you can pass variables to the functions, if you want to.
    let t          = FSMTable tr eff
    let patientFSM = FSMHandle st st t 90 3 :: FSMHandle PostgresJSONStore PostgresJSONStore SSN PatientStatus Event Action

    -- `post` gives you the flexibility of having different start states.
    post patientFSM "123-12-1235" (PatientAdmitted 0 [])
    res <- mkMsgs [Operation (Removed Arm)] >>= patch patientFSM "123-12-1235"

    get patientFSM "123-12-1235"  -- Just (PatientAdmitted 5000 [Removed Arm])
```


### Reliability
You may have noticed up there, that "patches" are wrapped in Msgs. They are used to give certain reliability guarantees in Mealstrom.

The `FSMAPI` through which you should interact with instances guarantees idempotance. `get` is trivially idempotent, `post` will let you know if the instance already exists and it is safe to retry. Finally, for `patch` you generate a `Msg` using `mkMsg` or `mkMsgs` that wraps an `Event` you want to send to an instance.

Once `patch` returns `True`, you can be assured that the state transition has occurred and the associated Actions are now running asynchronously. You can safely retry `patch`, because when a `msgId` is already known, the message is discarded.

You can run arbitrary effects, they will be retried until a retry limit you set is hit or until they succeed. This means they may happen [more than once] (https://en.wikipedia.org/wiki/Two_Generals'_Problem) or not at all. Failed effects can be retried at any time by calling `recoverAll`.

If, however, you choose to send a Msg to another _MealyInstance_ as an effect, i.e. call `patch` on it in the `effects` function, you can reuse the `msgId` from the first `Msg`. The receiving FSM instance can then even do the same thing, and so on. This way you can form a chain of idempotent updates that will, assuming failures are intermittent, eventually succeed.

### Log
The `FSMAPI` attempt to provide an exception-safe way to work with FSM instances in production. If you want to examine an instances log or alter the past, you can use the functions from the respective stores directly, but have to take care of exceptions yourself.

Lastly, Mealstrom is not a good fit if:

* You require every last bit of performance.
* You do not care particularly whether updates are occasionally lost.
* You require complex, cross-entity queries and/or already have a large amount of query language code, so that the drawbacks cited above do not seem too bad in comparison.
