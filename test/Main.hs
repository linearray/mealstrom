import BasicFSM   (runBasicTests)
import FSM2FSM    (runFSM2FSMTests)
import CounterFSM (runCounterTests)
import Recovery   (runRecoveryTests)
import Timeout    (runTimeoutTests)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Data.ByteString.Char8             as DBSC8

import Test.Tasty

main :: IO ()
main =
    let c = "host='localhost' port=5432 dbname='fsmtest'" in do
        conn <- connectPostgreSQL (DBSC8.pack c)
        _    <- execute_ conn $ Query (DBSC8.pack "DROP SCHEMA public CASCADE; CREATE SCHEMA public;")

        defaultMain $ testGroup "All tests" [
            runBasicTests c,
            runFSM2FSMTests c,
            runCounterTests c,
            runRecoveryTests c,
            runTimeoutTests c
            ]
