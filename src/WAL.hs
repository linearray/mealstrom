{-# LANGUAGE DataKinds #-}

module WAL where

import Data.UUID

import PostgresqlStore

openTxn :: PostgresqlStore WAL -> UUID -> IO ()
openTxn st i = walUpdate st i (+1)

closeTxn :: PostgresqlStore WAL -> UUID -> IO ()
closeTxn st i = walUpdate st i (+ (-1))
