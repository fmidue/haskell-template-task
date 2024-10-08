{-# LANGUAGE ScopedTypeVariables #-}
module TestHelper (
  isDefined, isDeeplyDefined, mustFail,
  qcWithTimeout, qcWithTimeoutAndArgs, qcWithTimeoutAndRuns
  , qc, qc', qcWithArgs --DEPRECATED
  , tcWithTimeout, tcWithTimeoutAndArgs, tcCustomizedWithTimeoutAndArgs, tcWithInputsOnFailure
  ) where
import Prelude (
  Bool (..), Either (Left, Right), Int, IO, String, Maybe (Nothing, Just),
  const, error, return, seq, ($), (++), show, (.), id)

import Control.Exception
  (ErrorCall, SomeException, catch, evaluate, try)
import Test.HUnit                       (Assertion, assertFailure)
import Test.QuickCheck
  (Args, Property, Result (GaveUp, Failure, Success), Testable,
   chatty, failingTestCase, maxSuccess, output,
   quickCheckWithResult, stdArgs, within)
import Test.QuickCheck.Monadic          (monadicIO, run)
import Control.DeepSeq                  (NFData, deepseq)

import Test.IOTasks (
  IOrep,
  Specification,
  taskCheckWithOutcome,
  feedbackStyle,
  printOutcomeWith,
  terminalOutput,
  )
import qualified Test.IOTasks as IOTasks (
  Args,
  CoreOutcome (..),
  Outcome (..),
  stdArgs,
  )
import qualified System.Timeout as System (timeout)

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

-- helper for new IOTasks implementation
tcWithTimeout :: Int -> IOrep () -> Specification -> Assertion
tcWithTimeout to = tcWithTimeoutAndArgs to IOTasks.stdArgs

tcWithTimeoutAndArgs :: Int -> IOTasks.Args -> IOrep () -> Specification -> Assertion
tcWithTimeoutAndArgs to args prog spec = tcCustomizedWithTimeoutAndArgs to args prog spec id

tcCustomizedWithTimeoutAndArgs :: Int -> IOTasks.Args -> IOrep () -> Specification -> (String -> String) -> Assertion
tcCustomizedWithTimeoutAndArgs to args prog spec transfom = tcTimeoutAndArgsHandleFailure to args prog spec (transfom . defaultErrorMessage args)

tcWithInputsOnFailure :: Int -> IOTasks.Args -> IOrep () -> Specification -> ([String] -> String) -> Assertion
tcWithInputsOnFailure to args prog spec withInputs =  tcTimeoutAndArgsHandleFailure to args prog spec handleFailure
 where
  handleFailure (IOTasks.Outcome (IOTasks.Failure inputs _ _ _) _) = withInputs inputs
  handleFailure _ = error "only called with failures"

defaultErrorMessage :: IOTasks.Args -> IOTasks.Outcome -> String
defaultErrorMessage args = show . printOutcomeWith (feedbackStyle args)

tcTimeoutAndArgsHandleFailure :: Int -> IOTasks.Args -> IOrep () -> Specification -> (IOTasks.Outcome -> String) -> Assertion
tcTimeoutAndArgsHandleFailure to args prog spec withFailure = do
  outcome <- System.timeout to $ taskCheckWithOutcome args{ terminalOutput = False } prog spec
  case outcome of
    Just (IOTasks.Outcome IOTasks.Success{} _) -> return ()
    Just (IOTasks.Outcome IOTasks.GaveUp _) -> assertFailure "Gave up on testing. This is usually not caused by a fault within your solution. Please contact your lecturers"
    Just o@(IOTasks.Outcome IOTasks.Failure{} _) -> assertFailure $ withFailure o
    Nothing -> assertFailure "Failure: Timeout"
