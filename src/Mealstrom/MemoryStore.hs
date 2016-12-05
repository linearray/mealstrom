{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Mealstrom.MemoryStore
Description : A memory-only storage backend, using STM.
Copyright   : (c) Max Amanshauser, 2016
License     : MIT
Maintainer  : max@lambdalifting.org
-}
module Mealstrom.MemoryStore (
    MemoryStore,
    mkStore,
    printWal
) where

import           Control.Concurrent.STM
import           Control.Exception
import           Data.Text
import           Data.Time
import qualified ListT
import           STMContainers.Map as Map

import           Mealstrom.FSM
import           Mealstrom.FSMStore
import           Mealstrom.WALStore

instance (MealyInstance k s e a) => FSMStore (MemoryStore k s e a) k s e a where
    fsmRead st k _p = do
        may <- atomically (_fsmRead st k)
        return $ fmap (currState . machine) may
    fsmCreate st a  = atomically $ _fsmCreate st a
    fsmUpdate st k t = _fsmUpdate st k t

instance WALStore (MemoryStore k s e a) k where
    walUpsertIncrement      =              Mealstrom.MemoryStore.walUpsertIncrement
    walDecrement       st k = atomically $ Mealstrom.MemoryStore.walDecrement st k
    walScan                 =              Mealstrom.MemoryStore.walScan

data MemoryStore k s e a where
    MemoryStore :: (MealyInstance k s e a) => {
        memstoreName    :: Text,
        memstoreBacking :: Map k (Instance k s e a),
        memstoreLocks   :: Map k (TMVar ()),
        memstoreWals    :: Map k (UTCTime,Int)
    } -> MemoryStore k s e a

_fsmRead :: MemoryStore k s e a -> k -> STM (Maybe (Instance k s e a))
_fsmRead MemoryStore{..} k = Map.lookup k memstoreBacking >>= \case
    Just a -> return $ Just a
    _      -> return Nothing

_fsmCreate :: MemoryStore k s e a -> Instance k s e a -> STM ()
_fsmCreate MemoryStore{..} ins = do
    t <- newTMVar ()
    Map.insert t   (key ins) memstoreLocks
    Map.insert ins (key ins) memstoreBacking

-- |We need to use a lock here, because we are in the unfortunate position of
-- having to use IO while performing STM operations, which is not possible.
-- Using the lock we can rest assured no concurrent update operation can progress.
_fsmUpdate :: MemoryStore k s e a -> k -> MachineTransformer s e a -> IO OutboxStatus
_fsmUpdate MemoryStore{..} k t =
    let
        m  = memstoreBacking
        ls = memstoreLocks
    in
        atomically (Map.lookup k ls) >>= \lock ->
            maybe (return NotFound)
                  (\l ->
                      bracket_ (atomically $ takeTMVar l)
                               (atomically $ putTMVar l ())
                               (atomically (Map.lookup k m) >>= \res ->
                                   maybe (return NotFound)
                                         (\inst ->
                                             (do
                                                 newMach <- t (machine inst)
                                                 let r = if Prelude.null (outbox newMach) then Done else Pending
                                                 atomically $ Map.insert inst{machine=newMach} k m
                                                 return r
                                             )
                                         ) res)
                  )
                  lock

walUpsertIncrement :: MemoryStore k s e a -> k -> IO ()
walUpsertIncrement MemoryStore{..} k =
    getCurrentTime >>= \t -> atomically $
        Map.lookup k memstoreWals >>= \res ->
            maybe (Map.insert (t,1) k memstoreWals)
                  (\(_oldt,w) -> Map.insert (t,w+1) k memstoreWals)
                  res

walDecrement :: MemoryStore k s e a -> k -> STM ()
walDecrement MemoryStore{..} k =
    Map.lookup k memstoreWals >>= \res ->
        maybe (error "trying to recover non-existing entry")
              (\(t,w) -> Map.insert (t,w-1) k memstoreWals)
              res

walScan :: MemoryStore k s e a -> Int -> IO [WALEntry k]
walScan MemoryStore{..} cutoff =
    getCurrentTime >>= \t -> atomically $
        let xx = addUTCTime (negate (fromInteger (toInteger cutoff) :: NominalDiffTime)) t in

        ListT.fold (\acc (k,(t,w)) -> if t < xx
                                    then return (WALEntry k t w : acc)
                                    else return acc) [] (stream memstoreWals)


printWal :: MemoryStore k s e a -> k -> IO ()
printWal MemoryStore{..} k =
    atomically (Map.lookup k memstoreWals) >>= \res ->
        maybe (putStrLn "NO WAL")
              print
              res


mkStore :: (MealyInstance k s e a) => Text -> IO (MemoryStore k s e a)
mkStore name = atomically $ do
    back  <- new :: STM (Map k (Instance k s e a))
    locks <- new :: STM (Map k (TMVar ()))
    wals  <- new :: STM (Map k (UTCTime,Int))
    return $ MemoryStore name back locks wals
