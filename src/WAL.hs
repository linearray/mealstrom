{-# LANGUAGE DataKinds #-}

module WAL where

import Data.UUID

import PostgresqlStore

openTxn :: PostgresqlStore WAL -> UUID -> IO ()
openTxn st i = walUpsert st i (+1)

closeTxn :: PostgresqlStore WAL -> UUID -> IO ()
closeTxn st i = walUpsert st i (+ (-1))
