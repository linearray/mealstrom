{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE ExplicitTypeApplication #-}

module MemoryStore where

import FSM

import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Text
import           Data.UUID
import           Data.UUID.V4
import           System.IO.Unsafe

data MemoryStore (v::IdStatus) s e a = MemoryStore {
    memstoreName     :: Text,
    memstoreStrategy :: Strategy,
    memstoreBacking  :: Map UUID (Instance v s e a)
}

fsmRead :: MemoryStore s e a -> UUID -> Maybe (Instance s e a)
fsmRead mst k = Map.lookup k (memstoreBacking mst)

fsmCreate :: MemoryStore s e a -> Instance s e a -> MemoryStore s e a
fsmCreate mst i =
    let m = memstoreBacking mst in
    case Map.lookup (uuid i) m of
        Nothing -> mst {memstoreBacking = Map.insert (uuid i) i m}
        Just i  -> mst

fsmUpdate :: MemoryStore s e a -> UUID -> PureTransformer s e a -> MemoryStore s e a
fsmUpdate mst i t =
    let
        m         = memstoreBacking mst
        newT inst = Just inst {machine = t (machine inst)}
     in
        mst {memstoreBacking = Map.update newT i m}

mkStore :: Text -> Strategy -> MemoryStore s e a
mkStore name strat = MemoryStore name strat (Map.empty :: Map UUID (Instance s e a))

data States = A | B | C   deriving (Show,Eq,Ord)
data Events = Msg1 | Msg2 deriving (Show,Eq)
data Action = Act1        deriving (Show,Eq)

testme =
    let
        mystore = mkStore "myTestBucket" EvalAndApplyNow :: MemoryStore Valid States Events Action
        u = unsafePerformIO nextRandom
        m = fsmRead mystore u :: Maybe ( Instance Valid States Events Action )
    in
        m
