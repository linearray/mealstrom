{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module FSM where

import Control.Monad (liftM, ap)
import Data.Aeson
import Data.Foldable (asum)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time
import Data.Typeable (Typeable, typeOf)
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

data Strategy = EvalNow | EvalLater

type Transformer     s e a = Machine s e a -> IO (Machine s e a)
type PureTransformer s e a = Machine s e a ->     Machine s e a

data WALEntry = WALEntry {
    walId    :: UUID,
    walTime  :: UTCTime,
    walCount :: Int
} deriving (Show,Eq,Generic,Typeable,ToJSON,FromJSON)

-- ######################
-- # Machine Definitions
-- ######################
-- These defintions are concerned with the basic functions of
-- finite state machines, keeping a memory and state transitions.

-- | A change in the FSM is either a (Step Timestamp oldState event newState Actions)
-- or an increase in a counter.
data Change s e a = Step Int s e s [a] | Count Int deriving (Show)
_inc (Count i)    = Count (i+1)

-- | Steps are equal to each other when they originated in the same state
-- received the same event and ended up in the same state
instance (Eq s, Eq e) => Eq (Change s e a) where
    (==) (Count a) (Count b) = a == b
    (==) (Step _ os1 e1 ns1 _) (Step _ os2 e2 ns2 _) = (os1 == os2) && (e1 == e2) && (ns1 == ns2)

data IdStatus = Valid | Invalid deriving (Show,Eq)

-- |One instance is akin to one entry in a DB table.
-- Instances are identified by a UUID.
data Instance s e a = Instance {
    uuid    :: UUID,
    machine :: Machine s e a
} deriving (Eq,Show,Generic,Typeable)

-- |Data structure used for persisting to disk.
data Machine s e a = Machine {
    inbox     :: [Msg e],
    outbox    :: [Msg a],
    committed :: [UUID],
    initState :: s,
    currState :: s,
    hist      :: [Change s e a]
} deriving (Eq,Show,Generic,Typeable)

mkEmptyMachine :: s -> Machine s e a
mkEmptyMachine s = Machine [] [] [] s s []

mkEmptyInstance :: s -> IO (Instance s e a)
mkEmptyInstance s = nextRandom >>= \u -> return $ Instance u (mkEmptyMachine s)

mkInstance :: UUID -> s -> [Msg e] -> Instance s e a
mkInstance i s es = Instance i ((mkEmptyMachine s) {inbox = es})


-- |Type of messages that are sent between FSMs
-- Messages are identified by UUID.
-- The purpose of Msg is to attach a unique ID to an event.
data Msg e = Msg {
    msgID       :: Maybe UUID,
    msgContents :: e
} deriving (Show,Eq,Generic)

mkMsg :: t -> IO (Msg t)
mkMsg t = nextRandom >>= \i -> return $ Msg (Just i) t

mkMsgs :: [t] -> IO [Msg t]
mkMsgs = mapM mkMsg

mkBogusMsg :: (Eq t) => t -> Msg t
mkBogusMsg = Msg Nothing

-- |Append a Change to a history.
-- Identical steps are just counted, otherwise they are consed to the history.
_append :: (Eq s, Eq e) => Change s e a -> [Change s e a] -> [Change s e a]
_append s1 all@(Count i:s2:rest)
    | s1 == s2 = Count (i+1):s2:rest
    | otherwise = s1 : all
_append s1 all@(s2:rest)
    | s1 == s2 = Count 1 : all
    | otherwise = s1 : all
_append s ss = s:ss


-- ##############
-- # JSON Codecs
-- ##############
instance ToJSON UUID where
    toJSON u = toJSON (toText u)

instance FromJSON UUID where
    parseJSON = withText "UUID" $ \x -> return . fromJust $ fromText x

instance (ToJSON s, ToJSON e, ToJSON a) => ToJSON (Change s e a) where
    toJSON (Count i) = object [ "count" .= toJSON i]
    toJSON (Step ts os ev ns as) =
        object [
            "timestamp" .= toJSON ts,
            "old_state" .= toJSON os,
            "event"     .= toJSON ev,
            "new_state" .= toJSON ns,
            "actions"   .= toJSON as
        ]

instance (FromJSON s, FromJSON e, FromJSON a) => FromJSON (Change s e a) where
    parseJSON =
        withObject "Change" $ \o ->
            asum [
                Count <$> o .: "count",
                Step  <$> o .: "timestamp" <*> o .: "old_state" <*> o .: "event" <*> o .: "new_state" <*> o .: "actions"
            ]
