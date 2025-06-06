{-# LANGUAGE ScopedTypeVariables #-}
module TestHarness (
  allowFailures,
  contains,
  findTopLevelDeclsOf,
  ident,
  randomChoice,
  run,
  syntaxCheck,
  syntaxCheckWithExts,
  ) where

import Prelude
  (Bool (..), Int, IO, Maybe (..), ShowS, String,
   ($), (>), (-), (+), (.), (++), (!!), (==), (||),
   concat, const, either, error, fmap, filter, foldr, id, length,
   map, mapM, maybe, otherwise, return, show)

import qualified System.IO              as IO (readFile) {- needed to avoid encoding problems -}
import qualified Test.HUnit             as HU
  (Assertion, Counts, Test (..), Testable, assertFailure, test)

import Control.Applicative              ((<$>))
import Control.Exception
  (ArithException, ArrayException, ErrorCall, Handler (Handler),
   PatternMatchFail, SomeException, catches, displayException, try)
import Control.Monad                    (foldM, when)
import Data.Generics                    (Data, Typeable, everything, mkQ)
import Data.List                        (intercalate, null)
import Language.Haskell.Exts
  (Decl (..), Match (..) , Module (..), Name (..), ParseResult (..), Pat (..),
   SrcSpanInfo, classifyExtension, parseFileContentsWithExts)
import System.IO.Unsafe                 (unsafePerformIO) -- We need to run the tests inside the interpreter
import System.Random                    (randomRIO)
import Test.HUnit.Base
  (Node (Label), Test, Counts (Counts), errors, failures, path, performTest)
import Test.HUnit.Text                  (showPath)

{-| Function called by the interpreter, getting the tests to run as the argument. -}
run :: HU.Testable t => [t] -> (HU.Counts, ShowS)
run testables =
  unsafePerformIO $
    catches
    (foldM performTestUnlessError (Counts 0 0 0 0, id) testables)
    [Handler $ \(e :: ErrorCall)        -> pairWith e,
     Handler $ \(e :: PatternMatchFail) -> pairWith e,
     Handler $ \(e :: ArithException)   -> pairWith e,
     Handler $ \(e :: ArrayException)   -> pairWith e]
  where
    pairWith e  =
      return (Counts 1 1 1 0,
              \a -> "Encountered runtime error during testing:\n" ++ displayException e ++ a)
    performTestUnlessError (cs, s) t
      | errors cs > 0 || failures cs > 0
      = return (cs, s)
      | otherwise
      = do (cs', s') <- runTestText id (HU.test t)
           return (sumCounts cs cs', s' . s)
    sumCounts (Counts c1 t1 e1 f1) (Counts c2 t2 e2 f2) =
      Counts (c1 + c2) (t1 + t2) (e1 + e2) (f1 + f2)

{-|
Perform test runs but change the format of output (based on
https://hackage.haskell.org/package/HUnit-1.6.0.0/docs/src/Test-HUnit-Text.html#runTestText
but specified to ShowS)
-}
runTestText :: ShowS -> Test -> IO (Counts, ShowS)
runTestText = performTest reportStart reportError reportFailure
  where
    reportStart _ = return
    reportError   = reportProblem "Error:"   "Error in:   "
    reportFailure = reportProblem "Failure:" "Failure in: "
    reportProblem p0 p1 _ msg ss us = return $ \rest -> us (line ++ '\n' : rest)
      where
        line  = "### " ++ kind ++ path' ++ "\n" ++ msg
        kind  = if null path' then p0 else p1
        path' = showPath [x | x@(Label _) <- path ss]

{-* Common test patterns -}

{-|
Detailed output of correct/incorrect Tests in case of failure
with the option to allow a fixed number of tests to fail.
-}
allowFailures :: Int -> [HU.Test] -> HU.Assertion
allowFailures limit testCases = do
  (successes, failures') <- collectResults [(l,a) | HU.TestLabel l (HU.TestCase a) <- testCases]
  when (length failures' > limit) $
    HU.assertFailure $ intercalate "\n\n"
      [ "Too many errors occurred. There are still functions that may be implemented as required."
      , "Correct are: " ++ intercalate ", " (map show successes)
      , "The following errors occurred:"
      , intercalate "\n---\n" [ "# " ++ l ++ ":\n" ++ show f | (l,f) <- failures' ]
      ]
  where
    collectResults = fmap (groupIntoTwoLists . concat)
                   . mapM (\(l, action) -> either (\(e :: SomeException) -> [(l,Just e)]) (const [(l,Nothing)]) <$> try action)
    groupIntoTwoLists :: [(a,Maybe b)] -> ([a],[(a,b)])
    groupIntoTwoLists = foldr (\(a,mb) (ns,js) -> maybe (a:ns,js) (\b -> (ns,(a,b):js)) mb) ([],[])

randomChoice :: [a] -> IO a
randomChoice xs = do
  r <- randomRIO (0, length xs - 1)
  return $ xs !! r

{-* Syntax predicates -}
contains :: (Typeable b, Data a) => (b -> Bool) -> a -> Bool
contains pred = everything (||) (mkQ False pred)

ident :: String -> Name SrcSpanInfo -> Bool
ident name (Ident _ name')  | name == name'           = True
ident name (Symbol _ name') | name == "("++name'++")" = True
ident _    _                                          = False

syntaxCheck :: (Module SrcSpanInfo -> HU.Assertion) -> HU.Assertion
syntaxCheck = syntaxCheckWithExts []

syntaxCheckWithExts :: [String] -> (Module SrcSpanInfo -> HU.Assertion) -> HU.Assertion
syntaxCheckWithExts exts check = do
  contents <- IO.readFile "Solution.hs"
  let mod = case parseFileContentsWithExts (map classifyExtension exts) contents of
              ParseOk mod'    -> mod'
              ParseFailed l e -> error $ "Parsing file contents failed at " ++ show l ++ ": " ++ e
  check mod

findTopLevelDeclsOf :: String -> Module SrcSpanInfo -> [Decl SrcSpanInfo]
findTopLevelDeclsOf _    XmlPage   {} = [] -- ignore non modules
findTopLevelDeclsOf _    XmlHybrid {} = [] -- ignore non modules
findTopLevelDeclsOf name (Module _ _ _ _ decls) = filter matches decls
  where
    matches (PatBind _ (PVar _ (Ident _ name')) _ _)      | name == name' = True
    matches (FunBind _ (Match _ (Ident _ name') _ _ _:_)) | name == name' = True
    matches _                                                             = False
