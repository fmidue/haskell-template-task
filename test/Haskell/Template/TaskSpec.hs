{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Haskell.Template.TaskSpec where

import qualified Data.ByteString.Char8            as BS (unpack)
import qualified Data.String.Interpolate          as SI (__i, i)
import qualified Text.PrettyPrint.Leijen.Text     as PP

import Haskell.Template.Task

import Control.Arrow                    ((+++))
import Control.Monad.Catch (
  Exception,
  MonadCatch (..),
  MonadThrow (..),
  )
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Writer       (runWriterT, tell)
import Data.Functor.Identity            (Identity (..))
import Data.List                        (intercalate, isPrefixOf)
import Data.List.Extra                  (split)
import Data.Maybe                       (fromJust)
import Data.Text.Lazy                   (unpack)
import Data.Yaml                        (encode)
import System.Directory
  (getTemporaryDirectory, setCurrentDirectory)
import System.FilePath                  ((</>))
import System.IO.Temp                   (withTempDirectory)
import Test.Hspec

newtype CustomException = CustomException PP.Doc
  deriving Show

instance Exception CustomException

defaultConfig :: SolutionConfig
defaultConfig = fromJust $ toMaybeSolutionConfig
  ( defaultSolutionConfig {
      addCodeWorldRenderButton = Just False,
      configGhcErrors = Just [],
      configGhcWarnings = Just [],
      configHlintErrors = Just [],
      configHlintSuggestions = Just [],
      provideSampleSolution = Just True,
      rigorousValidation = Just True } )

withHlintSuggestions :: Monad m => FSolutionConfig m -> [String] -> FSolutionConfig m
withHlintSuggestions config xs = config { configHlintSuggestions = return xs }

withHlintErrors :: Monad m => FSolutionConfig m -> [String] -> FSolutionConfig m
withHlintErrors config xs = config { configHlintErrors = return xs}

withHlintRules :: Monad m => FSolutionConfig m -> [String] -> FSolutionConfig m
withHlintRules config xs = config { configHlintRules = return xs}

completedDefaultCode :: String
completedDefaultCode = toCode defaultConfig . drop 1
  $ map unlines $ split ("---" `isPrefixOf`) $ lines defaultCode

spec :: Spec
spec = do
  describe "hlintFeedback" $ do
    it "accepts code without warnings/errors" $
      hlintIO defaultConfig noError errorP configHlintErrors `shouldReturn` []
    it "returns specified warnings" $
      hlintIO idSuggestion useId infoP configHlintSuggestions
      `shouldReturn` [Right "Warning: Redundant id"]
    it "returns specified errors" $
      hlintIO idError useId errorP configHlintErrors `shouldReturn` [Left "Warning: Redundant id"]
    it "returns no errors if specified warnings only" $
      hlintIO idSuggestion useId errorP configHlintErrors `shouldReturn` []
    it "returns no warnings if specified errors only" $
      hlintIO idError useId infoP configHlintSuggestions `shouldReturn` []
    it "returns specified custom warnings" $
      hlintIO dilatedError useDilated errorP configHlintErrors
      `shouldReturn` [Left "Warning: Use dilated"]
    it "returns specified custom warnings with name" $
      hlintIO dilatedNamed useDilated errorP configHlintErrors
      `shouldReturn` [Left "Warning: Do not use scaled"]
    it "allows specifying fixity" $
      hlintIO dilatedWithFixity useDilated errorP configHlintErrors
      `shouldReturn` [Left "Warning: Use dilated"]
  describe "maybe feedback" $ do
    it "should suggest replacing if with case-of when applicable (isJust)" $ do
        result <- maybeCheckIO False False (Just "Is there maybe a better way to express this?") [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if isNothing mVal then -3 else fromJust mVal|]

        result `shouldBe` [
          [SI.i|Found:
  if isNothing mVal then -3 else fromJust mVal

Is there maybe a better way to express this?
|]
          ]
    it "should suggest replacing if with case-of when applicable (isNothing)" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if mVal == Nothing then -3 else fromJust mVal|]

        length result `shouldBe` 1
    it "should suggest replacing if with case-of when applicable (== Nothing)" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if mVal /= Nothing then fromJust mVal else -3|]

        length result `shouldBe` 1
    it "should suggest replacing if with case-of when applicable (/= Nothing)" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if isJust mVal then fromJust mVal else -3|]

        length result `shouldBe` 1
    it "should not suggest replacing if with case-of when condition is mixed" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if isJust mVal && True then fromJust mVal else -3|]

        length result `shouldBe` 0
    it "should not suggest replacing if with case-of when condition is comparison with Just value" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if mVal == Just 5 then fromJust mVal else -3|]

        length result `shouldBe` 0
    it "should not suggest replacing if with case-of when value is not used" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if isJust mVal then 5 else -3|]

        length result `shouldBe` 0
    it "replacement should not introduce variable names that are already used or will be used later" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def mVal = let value = -3 in if isJust mVal then fromJust mVal else value

  def2 mVal = if isJust mVal then let value = 5 in fromJust mVal else -3

  def3 mVal = if isJust mVal then fromJust mVal * value else 0
    where value = 17

  def4 val = case val of
    [value] -> if isJust value then 2 * fromJust value else -1
    _ -> 0

  def5 mVal f = if isJust mVal then f (\\value -> fromJust mVal value) else undefined

  def6 mVal x = if isJust mVal then 2 - fromJust mVal else process x
    where
      process value = 3 * abs value

  def7 mVal xs = if isJust mVal then [fromJust mVal value | value <- xs] else xs

  def8 mVal xs = if isJust mVal
    then do
      value <- xs
      pure $ fromJust mVal value
    else xs
  |]

        result `shouldBe` [
          [SI.i|Found:
  if isJust mVal then fromJust mVal else value
Perhaps:
  case mVal of
    Nothing -> value
    Just value' -> value'|],
          [SI.i|Found:
  if isJust mVal then let value = 5 in fromJust mVal else -3
Perhaps:
  case mVal of
    Nothing -> -3
    Just value' -> let value = 5 in value'|],
          [SI.i|Found:
  if isJust mVal then fromJust mVal * value else 0
Perhaps:
  case mVal of
    Nothing -> 0
    Just value' -> value' * value|],
          [SI.i|Found:
  if isJust value then 2 * fromJust value else -1
Perhaps:
  case value of
    Nothing -> -1
    Just value' -> 2 * value'|],
          [SI.i|Found:
  if isJust mVal then f (\\ value -> fromJust mVal value) else
  undefined
Perhaps:
  case mVal of
    Nothing -> undefined
    Just value' -> f (\\ value -> value' value)|],
          [SI.i|Found:
  if isJust mVal then 2 - fromJust mVal else process x
Perhaps:
  case mVal of
    Nothing -> process x
    Just value' -> 2 - value'|],
          [SI.i|Found:
  if isJust mVal then [fromJust mVal value | value <- xs] else xs
Perhaps:
  case mVal of
    Nothing -> xs
    Just value' -> [value' value | value <- xs]|],
          [SI.i|Found:
  if isJust mVal then
  do value <- xs
     pure $ fromJust mVal value
  else xs
Perhaps:
  case mVal of
    Nothing -> xs
    Just value' -> do value <- xs
                      pure $ value' value|]
          ]
    it "should suggest replacement even when nested" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Maybe Int -> Int
  def mVal1 mVal 2 = if isJust mVal1 then fromJust mVal1 + if isJust mVal2 then 2 * fromJust mVal2 else 0 else -1|]

        result `shouldBe` [
          [SI.i|Found:
  if isJust mVal1 then
  fromJust mVal1 + if isJust mVal2 then 2 * fromJust mVal2 else 0
  else -1
Perhaps:
  case mVal1 of
    Nothing -> -1
    Just value -> value +
                    if isJust mVal2 then 2 * fromJust mVal2 else 0|],
          [SI.i|Found:
  if isJust mVal2 then 2 * fromJust mVal2 else 0
Perhaps:
  case mVal2 of
    Nothing -> 0
    Just value -> 2 * value|]
          ]
    it "should not suggest replacing if with case-of when isJust, fromJust, etc. are not from Data.Maybe" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if isJust mVal then fromJust mVal else -3
    where isJust _ = False

  def2 :: Maybe Int -> Int
  def2 mVal = if isJust mVal then fromJust mVal else -3
    where fromJust _ = 99|]

        length result `shouldBe` 0
    it "should not suggest replacing if with case-of when scrutinee is not a variable" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: a -> (a -> Maybe Int) -> Int
  def val f = if isJust (f val) then fromMaybe (f val) else -3|]

        length result `shouldBe` 0
    it "should not suggest replacing if with case-of when fromJust is used on shadowed maybe value" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if isJust mVal then let mVal = Just 99 in fromJust mVal else -3|]

        length result `shouldBe` 0
    it "replacement should not use redundant brackets" $ do
        result <- maybeCheckIO False True Nothing [SI.i|
  module Main where
  import Data.Maybe

  def :: Maybe Int -> Int
  def mVal = if (isJust (mVal)) then 1 + (fromJust ((mVal))) else -3|]

        result `shouldBe` [
          [SI.i|Found:
  if (isJust (mVal)) then 1 + (fromJust ((mVal))) else -3
Perhaps:
  case mVal of
    Nothing -> -3
    Just value -> 1 + value|]
          ]
  describe "grade" $ do
    it "is running" $
      gradeIO completedDefaultCode useImport `shouldReturn` ""
    it "allows syntaxCheck" $
      gradeIO (withSyntaxCheck True) useImport `shouldReturn` ""
    it "errors out on failing syntaxCheck" $
      exceptionToString (gradeIO (withSyntaxCheck False) useImport)
      `shouldReturn` [SI.__i|
          \#\#\# Failure in: 'r' does not use 'reverse'?
          expected: False
           but got: True
          |] ++ "\n"
    it "returns specified warnings" $
      exceptionToString (gradeIO withHlintError useId)
      `shouldReturn` [SI.__i|
         Main.hs:3:9-12: Warning: Redundant id
         Found:
           id x
         Perhaps:
           x
         |]
         ++ '\n' : rejectLine
    it "fails with forbidden warnings" $
      exceptionToString (gradeIO (toCode incompletePattern [useImport, tests]) useImport)
      `shouldReturn` [SI.__i|
         Solution.hs:5:1: error:
             Pattern match(es) are non-exhaustive
             In an equation for ‘incomplete’:
                 Patterns of type ‘[a]’ not matched: (_:_)

         Solution.hs:6:1: error:
             Pattern match(es) are non-exhaustive
             In an equation for ‘incomplete2’:
                 Patterns of type ‘[a]’ not matched: []
         |]
         ++ rejectLine
    it "adds additional message when trying to pattern-match with variable occurring multiple times" $
      exceptionToString (gradeIO completedDefaultCode patternWithDuplicateVariable)
      `shouldReturn` [SI.__i|
         Solution.hs:4:4: error:
             • Conflicting definitions for ‘x’
               Bound at: Solution.hs:4:4
                         Solution.hs:4:6
             • In an equation for ‘r’
             • Are you using pattern-matching with a pattern containing a variable with multiple occurrences?
         |] ++ "\n"
    it "adds additional message when trying to use Prolog cons operator" $
      exceptionToString (gradeIO completedDefaultCode prologListCons)
      `shouldReturn` [SI.__i|
         Solution.hs:4:15: error:
             • Couldn't match expected type ‘Bool’ with actual type ‘[a]’
             • In the expression: xs
               In a stmt of a list comprehension: xs
               In the expression: [x | xs]
             • Relevant bindings include
                 xs :: [a] (bound at Solution.hs:4:6)
                 x :: a (bound at Solution.hs:4:4)
                 r :: [a] -> [a] (bound at Solution.hs:4:1)
             • Are you trying to use '|' as a list constructor instead of ':'?
         |] ++ "\n"
    context "when submission is successful" $ do
      it "returns True and the configured custom message if a sample solution clone is submitted" $
        gradeIOWithRes withCloneMessage useImport `shouldReturn` (True, cloneMessage)
      it "returns False and no additional message if a clone is submitted, but the option is not set" $
        gradeIOWithRes completedDefaultCode useImport `shouldReturn` (False, "")
      it "returns False and no additional message if option is set, but submission is no clone" $
        gradeIOWithRes withCloneMessage noClone `shouldReturn` (False, "")
  where
    rejectLine = '\n' : '\n' : render rejectHint
    exceptionToString f = catch f (\(CustomException x) -> pure $ render x)
    withHlintError = toCode idError [useId]
    useImport = [SI.__i|
      module Solution where
      import Prelude
      r :: [a] -> [a]
      r = reverse
      incomplete [] = undefined
      incomplete2 (_:_) = undefined
      |]
    noClone = [SI.__i|
      module Solution where
      import Prelude
      r :: [a] -> [a]
      r [] = []
      r (x:xs) = reverse xs ++ [x]
      |]
    patternWithDuplicateVariable = [SI.__i|
      module Solution where
      import Prelude
      r :: [a] -> [a]
      r (x:x:xs) = xs
      |]
    prologListCons = [SI.__i|
      module Solution where
      import Prelude
      r :: [a] -> [a]
      r (x:xs) = [x|xs]
      |]
    withCloneMessage = toCode cloneMessageSetting $ program : tests : remaining
    (_ : program : tests : remaining) =
      map unlines $ split ("---" `isPrefixOf`) $ lines completedDefaultCode
    withSyntaxCheck withReverse = unlines $ intercalate ["-------"] $
      let (config : program : _ : remaining) =
            split ("---" `isPrefixOf`) $ lines completedDefaultCode
      in config : program : [syntaxCheck withReverse] : remaining
    syntaxCheck :: Bool -> String
    syntaxCheck withReverse = [SI.__i|
      module Test (test) where
      import Prelude
      import Test.HUnit ((~:),(@?=), Test)
      import TestHarness
      test :: [Test]
      test = [
        "'r' does #{negateString}use 'reverse'?" ~:
          syntaxCheck $ \\code ->
            contains (ident "reverse") (findTopLevelDeclsOf "r" code) @?= #{withReverse}
        ]
      |]
      where
        negateString
          | withReverse = "" :: String
          | otherwise = "not "
    incompletePattern = defaultConfig {
      configGhcErrors = pure ["incomplete-patterns"]
      }
    idSuggestion = defaultConfig
      `withHlintSuggestions` ["Redundant id"]
      `withHlintErrors` []
    idError      = defaultConfig
      `withHlintErrors` ["Redundant id"]
      `withHlintSuggestions` []
    dilatedError = defaultConfig
      `withHlintErrors` ["Use dilated"]
      `withHlintRules` ["warn: {lhs: scaled x x, rhs: dilated x}"]
    dilatedNamed = defaultConfig
      `withHlintErrors` ["Do not use scaled"]
      `withHlintRules` ["warn: {lhs: scaled x x, rhs: dilated x, name: Do not use scaled}"]
    dilatedWithFixity = defaultConfig
      `withHlintErrors` ["Use dilated"]
      `withHlintRules` ["fixity: infixr 0 &", "warn: {lhs: scaled x x, rhs: dilated x}"]
    cloneMessageSetting = defaultConfig
      {messageOnCloningSampleSolution = pure $ Just cloneMessage}
    cloneMessage = "Clone found"


toCode :: SolutionConfig -> [String] -> String
toCode config programs = intercalate "\n----\n" $ configText : programs
  where
    configText = BS.unpack . encode $ toSolutionConfigOpt config

gradeIO :: String -> String -> IO String
gradeIO task submission = snd <$> gradeIOWithRes task submission

gradeIOWithRes :: String -> String -> IO (Bool,String)
gradeIOWithRes task submission = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "Grade-test" $ \dir -> do
    setCurrentDirectory dir
    runWriterT $ grade
      id
      id
      (throwM . CustomException)
      (tell . show)
      dir
      task
      submission

hlintIO :: SolutionConfig
           -> String
           -> (PP.Doc -> IO (Either String String))
           -> (SolutionConfig -> Identity [String])
           -> IO [Either String String]
hlintIO config content display selectHints = do
  tmp <- liftIO getTemporaryDirectory
  withTempDirectory tmp "Template-test" $ \dir -> do
    setCurrentDirectory dir
    let file = dir </> "Main.hs"
    writeFile file content
    feedback <- getHlintFeedback display config tmp file selectHints
    return $ (repackStrings +++ repackStrings) <$> feedback
  where
    repackStrings x =
      let xs = filterWarnings $ lines x
      in if null xs then x else head xs
    filterWarnings = filter (not . null) . fmap compactWarnings
    compactWarnings s@('W':'a':'r':'n':'i':'n':'g':':':_) = firstLineOf s
    compactWarnings (_:ys) = compactWarnings ys
    compactWarnings []     = []
    firstLineOf ('\\':'n':_) = []
    firstLineOf (y:ys)            = y : firstLineOf ys
    firstLineOf []                = []

maybeCheckIO :: Bool -> Bool -> Maybe String -> String -> IO [String]
maybeCheckIO asError provideSuggestion additionalMessage code = do
  codeAST <- parse (fail . show) [] code
  let config = MaybeCaseOfOpportunityDetectionSetting {
      status = Identity $ if asError then Reject else Hint,
      provideSuggestion = Identity provideSuggestion,
      additionalMessage = Identity additionalMessage
    }
  getMaybeFeedback (pure . show) config codeAST

render :: PP.Doc -> String
render = unpack . PP.displayT . PP.renderPretty 1.0 100

errorP :: PP.Doc -> IO (Either String b)
errorP = return . Left . render

infoP :: PP.Doc -> IO (Either a String)
infoP = return . Right . render

noError :: String
noError = [SI.i|
foo :: Int -> Int
foo = id|]

useId :: String
useId = [SI.i|
foo :: Int -> Int
foo x = id x|]

useDilated :: String
useDilated = [SI.i|
foo :: Double -> Picture
foo x = scaled x x|]
