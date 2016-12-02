{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Machine Definitions
-- These defintions are concerned with the basic functions of
-- finite state machines, keeping a memory and state transitions.

module FSM where

import           Control.Monad (liftM, ap)
import           Data.Aeson
import           Data.Foldable (asum)
import           Data.Hashable
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Text (Text)
import           Data.Time
import           Data.Typeable (Typeable, typeOf)
import qualified Data.UUID as UUID
import           Data.UUID (UUID)
import           Data.UUID.V4
import           GHC.Generics
import           System.IO.Unsafe (unsafePerformIO)
import           TextShow hiding (fromText,toText)

type MachineTransformer s e a = Machine s e a -> IO (Machine s e a)
data OutboxStatus             = NotFound | Pending | Done deriving (Eq, Show)

class (FSMKey k) => MealyInstance k s e a

class (Hashable k, Eq k) => FSMKey k where
    toText   :: k -> Text
    fromText :: Text -> k

-- | A change in the FSM is either a (Step Timestamp oldState event newState Actions)
-- or an increase in a counter.
data Change s e a = Step Int s e s [a] | Count Int deriving (Show)
_inc (Count i)    = Count (i+1)

-- | Steps are equal to each other when they originated in the same state
-- received the same event and ended up in the same state
instance (Eq s, Eq e) => Eq (Change s e a) where
    (==) (Count a) (Count b) = a == b
    (==) (Step _ os1 e1 ns1 _) (Step _ os2 e2 ns2 _) = (os1 == os2) && (e1 == e2) && (ns1 == ns2)

-- |One instance is akin to one entry in a DB table.
-- Instances are identified by a UUID.
data Instance k s e a = Instance {
    key     :: k,
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

mkEmptyInstance :: k -> s -> Instance k s e a
mkEmptyInstance k s = Instance k (mkEmptyMachine s)

mkInstance :: k -> s -> [Msg e] -> Instance k s e a
mkInstance k s es = Instance k ((mkEmptyMachine s) {inbox = es})


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
    toJSON u = toJSON (UUID.toText u)

instance FromJSON UUID where
    parseJSON = withText "UUID" $ \x -> return . fromJust $ UUID.fromText x

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


-- Other Instances
instance FSMKey Text where
    toText   = id
    fromText = id

instance FSMKey UUID where
    toText     = UUID.toText
    fromText a = fromMaybe (error "Conversion from UUID failed") (UUID.fromText a)
