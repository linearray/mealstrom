import BasicFSM   (runBasicTests)
import FSM2FSM    (runFSM2FSMTests)
import CounterFSM (runCounterTests)
import Recovery   (runRecoveryTests)
import Timeout    (runTimeoutTests)
import Exception  (runExceptionTests)
import Upgrade    (runUpgradeTests)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Data.ByteString.Char8             as DBSC8
import Data.Maybe                           (fromMaybe)
import Data.Semigroup ((<>))

import System.Environment

import Test.Tasty

main :: IO ()
main = do
    h  <- fromMaybe "localhost" <$> lookupEnv "PGHOST"
    p  <- fromMaybe "5432"      <$> lookupEnv "PGPORT"
    u  <- fromMaybe "postgres"  <$> lookupEnv "PGUSER"
    pw <- fromMaybe ""          <$> lookupEnv "PGPASSWORD"

    let c = "host='" <> h <> "' port=" <> p <> " dbname='fsmtest' user='" <> u <> "' password='" <> pw <> "'"
    conn <- connectPostgreSQL (DBSC8.pack c)
    _    <- execute_ conn $ Query (DBSC8.pack "DROP SCHEMA public CASCADE; CREATE SCHEMA public;")

    defaultMain $ testGroup "All tests" [
        runBasicTests c,
        runFSM2FSMTests c,
        runCounterTests c,
        runRecoveryTests c,
        runTimeoutTests c,
        runExceptionTests c,
        runUpgradeTests c
        ]
