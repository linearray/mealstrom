import BasicFSM (runBasicTest)
import FSM2FSM (runFSM2FSMTest)
import CounterFSM (runCounterTest)
import Recovery (runRecoveryTest)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Data.ByteString.Char8 as DBSC8

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
    let c = "host='localhost' port=5432 dbname='fsmtest'" in do
        conn <- connectPostgreSQL (DBSC8.pack c)
        execute_ conn $ Query (DBSC8.pack "DROP SCHEMA public CASCADE; CREATE SCHEMA public;")

        defaultMain $ testGroup "All tests" [
            runBasicTest c,
            runFSM2FSMTest c,
            runCounterTest c,
            runRecoveryTest c
            ]
