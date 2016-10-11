import BasicFSM (runBasicTest)
import FSM2FSM (runFSM2FSMTest)
import CounterFSM (runCounterTest)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "All tests" [
    runBasicTest,
    runFSM2FSMTest,
    runCounterTest
    ]
