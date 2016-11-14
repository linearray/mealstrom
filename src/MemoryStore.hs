{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


-- |STM container for keeping mappings UUID -> Instance s e a
module MemoryStore (
    MemoryStore,
    mkStore,
    printWal
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Text
import           Data.Time
import           Data.UUID
import           Data.UUID.V4
import           Debug.Trace
import qualified ListT
import           STMContainers.Map as Map
import           System.IO.Unsafe

import FSM
import FSMStore
import WALStore


instance FSMStore (MemoryStore s e a) (Instance s e a) where
    fsmRead   st i   = atomically $ _fsmRead st i
    fsmCreate st a   = atomically $ _fsmCreate st a
    fsmUpdate        =              _fsmUpdate

instance WALStore (MemoryStore s e a) where
    walUpsertIncrement      =              MemoryStore.walUpsertIncrement
    walDecrement       st i = atomically $ MemoryStore.walDecrement st i
    walScan                 =              MemoryStore.walScan

data MemoryStore s e a = MemoryStore {
    memstoreName    :: Text,
    memstoreBacking :: Map UUID (Instance s e a),
    memstoreLocks   :: Map UUID (TMVar ()),
    memstoreWals    :: Map UUID (UTCTime,Int)
}

_fsmRead :: MemoryStore s e a -> UUID -> STM (Maybe (Instance s e a))
_fsmRead mst i = Map.lookup i (memstoreBacking mst) >>= \case
    Just a -> return $ Just a
    _      -> return Nothing

_fsmCreate :: MemoryStore s e a -> Instance s e a -> STM ()
_fsmCreate mst ins = do
    t <- newTMVar ()
    Map.insert t   (uuid ins) (memstoreLocks mst)
    Map.insert ins (uuid ins) (memstoreBacking mst)

-- |We need to use a lock here, because we are in the unfortunate position of
-- having to use IO while performing STM operations, which is not possible.
-- Using the lock we can rest assured no concurrent update operation can progress.
_fsmUpdate :: MemoryStore s e a -> UUID -> Transformer s e a -> IO OutboxStatus
_fsmUpdate mst i t =
    let
        m  = memstoreBacking mst
        ls = memstoreLocks mst
    in
        atomically (Map.lookup i ls) >>= \lock ->
            maybe (return NotFound)
                  (\l ->
                      bracket_ (atomically $ takeTMVar l)
                               (atomically $ putTMVar l ())
                               (atomically (Map.lookup i m) >>= \res ->
                                   maybe (return NotFound)
                                         (\inst ->
                                             (do
                                                 newInst <- t inst
                                                 let r = if Prelude.null (outbox (machine newInst)) then Done else Pending
                                                 atomically $ Map.insert newInst i m
                                                 return r
                                             )
                                         ) res)
                  )
                  lock

walUpsertIncrement :: MemoryStore s e a -> UUID -> IO ()
walUpsertIncrement mst i = let m = memstoreWals mst in
    getCurrentTime >>= \t -> atomically $
        Map.lookup i m >>= \res ->
            maybe (Map.insert (t,1) i m)
                  (\(t,w) -> Map.insert (t,w+1) i m)
                  res

walDecrement :: MemoryStore s e a -> UUID -> STM ()
walDecrement mst i = let m = memstoreWals mst in
    Map.lookup i m >>= \res ->
        maybe (error "trying to recover non-existing entry")
              (\(t,w) -> Map.insert (t,w-1) i m)
              res

walScan :: MemoryStore s e a -> Int -> IO [WALEntry]
walScan mst cutoff = let m = memstoreWals mst in
    getCurrentTime >>= \t -> atomically $
        let xx = addUTCTime (negate (fromInteger (toInteger cutoff) :: NominalDiffTime)) t in

        ListT.fold (\acc (k,(t,w)) -> if t < xx
                                    then return (WALEntry k t w : acc)
                                    else return acc) [] (stream m)


printWal :: MemoryStore s e a -> UUID -> IO ()
printWal mst i = let m = memstoreWals mst in
    atomically (Map.lookup i m) >>= \res ->
        maybe (putStrLn "NO WAL")
              print
              res


mkStore :: Text -> IO (MemoryStore s e a)
mkStore name = atomically $ do
    back  <- new :: STM (Map UUID (Instance s e a))
    locks <- new :: STM (Map UUID (TMVar ()))
    wals  <- new :: STM (Map UUID (UTCTime,Int))
    return $ MemoryStore name back locks wals
