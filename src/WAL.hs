module WAL where

import Data.UUID

import WALStore

openTxn :: WALStore st k => st -> k -> IO ()
openTxn = walUpsertIncrement

closeTxn :: WALStore st k => st -> k -> IO ()
closeTxn = walDecrement
