{-# LANGUAGE ScopedTypeVariables #-}
module TestHelper (
  isDefined, isDeeplyDefined, mustFail,
  qcWithTimeout, qcWithTimeoutAndArgs, qcWithTimeoutAndRuns
  , qc, qc', qcWithArgs --DEPRECATED
  ) where
import Prelude (
  Bool (..), Either (Left, Right), Int, IO, String,
  const, error, return, seq, ($), (++))

import Control.Exception
  (ErrorCall, SomeException, catch, evaluate, try)
import Test.HUnit                       (Assertion, assertFailure)
import Test.QuickCheck
  (Args, Property, Result (GaveUp, Failure, Success), Testable,
   chatty, failingTestCase, maxSuccess, output,
   quickCheckWithResult, stdArgs, within)
import Test.QuickCheck.Monadic          (monadicIO, run)
import Control.DeepSeq                  (NFData, deepseq)

qcWithArgs :: Testable prop => Int -> Args -> prop -> Assertion
qcWithArgs = qcWithTimeoutAndArgs

qcWithTimeoutAndArgs :: Testable prop => Int -> Args -> prop -> Assertion
qcWithTimeoutAndArgs timeout args prop = do
  result <- quickCheckWithResult (args {chatty = False}) (within timeout prop)
  assertFailureCustom result

qc' :: Testable prop => Int -> Int -> prop -> Assertion
qc' = qcWithTimeoutAndRuns

qcWithTimeoutAndRuns :: Testable prop => Int -> Int -> prop -> Assertion
qcWithTimeoutAndRuns timeout n = qcWithArgs timeout (stdArgs {maxSuccess = n})

qc :: Testable prop => Int -> prop -> Assertion
qc = qcWithTimeout

qcWithTimeout :: Testable prop => Int -> prop -> Assertion
qcWithTimeout timeout = qcWithTimeoutAndRuns timeout 1000

assertFailureCustom :: Result -> IO ()
assertFailureCustom Success {} = return ()
assertFailureCustom GaveUp {} = assertFailure $
  "Gave up on testing. This usually indicates that your code breaks an assumed invariant.\n"
  ++ "Perhaps you missed a detail within the task description."
assertFailureCustom Failure { failingTestCase = [t] } =
  assertFailure $ "Failing test case:\n" ++ t
assertFailureCustom x = assertFailure $ output x

mustFail :: a -> String -> Property
mustFail x msg =
  monadicIO $ run $ do
    resultOrError <- try (evaluate x)
    case resultOrError of
      Left (_::SomeException) -> return () -- expected failure occured.
      Right _                 -> error msg

isDeeplyDefined :: NFData a => a -> IO Bool
isDeeplyDefined x = catch
  (deepseq x $ return True)
  ((const $ return False) :: ErrorCall -> IO Bool)

isDefined :: a -> IO Bool
isDefined x = catch
  (seq x $ return True)
  ((const $ return False) :: ErrorCall -> IO Bool)
