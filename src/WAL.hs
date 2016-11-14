module WAL where

import Data.UUID

import WALStore

openTxn :: WALStore st => st -> UUID -> IO ()
openTxn = walUpsertIncrement

closeTxn :: WALStore st => st -> UUID -> IO ()
closeTxn = walDecrement
