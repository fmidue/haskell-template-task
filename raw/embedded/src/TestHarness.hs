{-# LANGUAGE ScopedTypeVariables #-}
module TestHarness (
  allowFailures,
  contains,
  doNotation,
  rhsContains,
  findDecls,
  findTopLevelDeclsOf,
  ident,
  listComprehension,
  randomChoice,
  run,
  selfRecursive,
  syntaxCheck,
  syntaxCheckWithExts,
  typeSignatureOf
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
import Data.Generics                    (Data, Typeable, everything, listify, mkQ)
import Data.List                        (intercalate, null)
import Language.Haskell.Exts
  (Decl (..), Exp (..), Match (..) , Module (..), Name (..), ParseResult (..), Pat (..),
   Rhs (..), SrcSpanInfo, classifyExtension, parseFileContentsWithExts)
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

{- |
Search a structure for a node satisfying some predicate.
Use case: scan submission code syntax tree.

Also matches the left hand side of function definitions,
so cannot be used to check if a function calls itself.

This will also match explicit imports
when searching for an identifier in the overall module via `ident`.
-}
contains :: (Typeable b, Data a) => (b -> Bool) -> a -> Bool
contains pred = everything (||) (mkQ False pred)

{- |
Same as `contains`, but only considers the right hand side of equations.
Can therefore be used to check for self recursion and avoids overlap with imports.
-}
rhsContains :: (Typeable b, Data a) => (b -> Bool) -> a -> Bool
rhsContains pred = contains (contains pred :: Rhs SrcSpanInfo -> Bool)

{-* Predicates to use with `contains` and `rhsContains` -}

{- |
True if identifier is the given String.
-}
ident :: String -> Name SrcSpanInfo -> Bool
ident name (Ident _ name')  | name == name'           = True
ident name (Symbol _ name') | name == "("++name'++")" = True
ident _    _                                          = False

{- |
True if expression is a list comprehension.
-}
listComprehension :: Exp SrcSpanInfo -> Bool
listComprehension (ListComp {}) = True
listComprehension _             = False

{- |
True if expression is a do block.
-}
doNotation :: Exp SrcSpanInfo -> Bool
doNotation (Do _ _) = True
doNotation _        = False

{- |
True if declaration is defined recursively.

This returns a false positive if a binding in the righthand side
uses the definition's name but goes unused.
-}
selfRecursive :: Decl SrcSpanInfo -> Bool
selfRecursive decl = case decl of
  (FunBind _ matches@(aMatch:_))  -> rhsContains (sameIdent $ matchName aMatch) matches
  (PatBind _ (PVar _ name) rhs _) -> contains (sameIdent name) rhs
  _                               -> False
  where
    matchName (Match _ n _ _ _)        = n
    matchName (InfixMatch _ _ n _ _ _) = n

    sameIdent (Ident _ n1) = ident n1
    sameIdent (Symbol _ n1) = ident $ "(" ++ n1 ++ ")"

{- |
True if declaration is a type signature of the given function name.
-}
typeSignatureOf :: String -> Decl SrcSpanInfo -> Bool
typeSignatureOf name (TypeSig _ xs _) = contains (ident name) xs
typeSignatureOf _    _                = False

{-* Performing syntax checks -}

{- |
Run an assertion on the submission module's syntax tree.

Only enables language extensions explicitly listed in the student's solution file.
Using any hidden default extensions will cause a parse error.
-}
syntaxCheck :: (Module SrcSpanInfo -> HU.Assertion) -> HU.Assertion
syntaxCheck = syntaxCheckWithExts []

{- |
Same as `syntaxCheck`, but takes a list of extensions to enable
on top of what is found in the file.
-}
syntaxCheckWithExts :: [String] -> (Module SrcSpanInfo -> HU.Assertion) -> HU.Assertion
syntaxCheckWithExts exts check = do
  contents <- IO.readFile "Solution.hs"
  let mod = case parseFileContentsWithExts (map classifyExtension exts) contents of
              ParseOk mod'    -> mod'
              ParseFailed l e -> error $ "Parsing file contents failed at " ++ show l ++ ": " ++ e
  check mod

{-* Syntax queries -}

{- |
Query a module's syntax tree for all top level function declarations and constants.
-}
findTopLevelDeclsOf :: String -> Module SrcSpanInfo -> [Decl SrcSpanInfo]
findTopLevelDeclsOf _    XmlPage   {} = [] -- ignore non modules
findTopLevelDeclsOf _    XmlHybrid {} = [] -- ignore non modules
findTopLevelDeclsOf name (Module _ _ _ _ decls) = filter matches decls
  where
    matches (PatBind _ (PVar _ (Ident _ name')) _ _)      | name == name' = True
    matches (FunBind _ (Match _ (Ident _ name') _ _ _:_)) | name == name' = True
    matches _                                                             = False

{- |
Query a syntax tree for all declarations and constants.
This includes let and where bindings.
-}
findDecls :: Data a => a -> [Decl SrcSpanInfo]
findDecls = listify $ const True
