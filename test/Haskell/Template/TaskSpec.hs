{-# LANGUAGE QuasiQuotes #-}
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
import Control.Monad.Trans.Writer       (execWriterT, tell)
import Data.List                        (intercalate, isPrefixOf)
import Data.List.Extra                  (split)
import Data.Maybe                       (fromJust)
import Data.Text.Lazy                   (unpack)
import Data.Yaml                        (encode)
import System.Directory
  (getTemporaryDirectory, setCurrentDirectory)
import System.IO.Temp                   (withTempDirectory)
import Test.Hspec

newtype CustomException = CustomException PP.Doc
  deriving Show

instance Exception CustomException

defaultConfig :: SolutionConfig
defaultConfig = fromJust $ finaliseConfigs [defaultSolutionConfig]

withHlintSuggestions :: Monad m => FSolutionConfig m -> [String] -> FSolutionConfig m
withHlintSuggestions config xs = config { configHlintSuggestions = return xs }

withHlintErrors :: Monad m => FSolutionConfig m -> [String] -> FSolutionConfig m
withHlintErrors config xs = config { configHlintErrors = return xs}

withHlintRules :: Monad m => FSolutionConfig m -> [String] -> FSolutionConfig m
withHlintRules config xs = config { configHlintRules = return xs}

spec :: Spec
spec = do
  describe "hlintFeedback" $ do
    it "accepts code without warnings/errors" $
      hlintIO defaultConfig noError True `shouldReturn` []
    it "returns specified warnings" $
      hlintIO idSuggestion useId False
      `shouldReturn` [Right "Warning: Redundant id"]
    it "returns specified errors" $
      hlintIO idError useId True `shouldReturn` [Left "Warning: Redundant id"]
    it "returns no errors if specified warnings only" $
      hlintIO idSuggestion useId True `shouldReturn` []
    it "returns no warnings if specified errors only" $
      hlintIO idError useId False `shouldReturn` []
    it "returns specified custom warnings" $
      hlintIO dilatedError useDilated True
      `shouldReturn` [Left "Warning: Use dilated"]
    it "returns specified custom warnings with name" $
      hlintIO dilatedNamed useDilated True
      `shouldReturn` [Left "Warning: Do not use scaled"]
    it "allows specifying fixity" $
      hlintIO dilatedWithFixity useDilated True
      `shouldReturn` [Left "Warning: Use dilated"]
  describe "grade" $ do
    it "is running" $
      gradeIO defaultCode useImport `shouldReturn` ""
    it "allows syntaxCheck" $
      gradeIO (withSyntaxCheck True) useImport `shouldReturn` ""
    it "error on failing syntaxCheck" $
      exceptionToString (gradeIO (withSyntaxCheck False) useImport)
      `shouldReturn` [SI.__i|
          \#\#\# Failure in: 'r' does not use 'reverse'?
          expected: False
           but got: True
          |] ++ "\n"
    it "returns specified warnings" $
      exceptionToString (gradeIO withHlintError useId)
      `shouldReturn` [SI.__i|
         3:9-12: Warning: Redundant id
         Found:
           id x
         Perhaps:
           x
         |]
         ++ '\n' : rejectLine
    it "fails with forbidden warnings" $
      exceptionToString (gradeIO (toCode incompletePattern [useImport, unlines tests]) useImport)
      `shouldReturn` [SI.__i|
         5:1: error:
             Pattern match(es) are non-exhaustive
             In an equation for ‘incomplete’:
                 Patterns of type ‘[a]’ not matched: (_:_)
         6:1: error:
             Pattern match(es) are non-exhaustive
             In an equation for ‘incomplete2’:
                 Patterns of type ‘[a]’ not matched: []
         |]
         ++ rejectLine
  where
    rejectLine = '\n' : render rejectHint
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
    (config : program : tests : remaining) =
      split ("---" `isPrefixOf`) $ lines defaultCode
    withSyntaxCheck withReverse = unlines $ intercalate ["-------"] $
      let (config : program : _ : remaining) =
            split ("---" `isPrefixOf`) $ lines defaultCode
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
          syntaxCheck $ \\modul ->
            contains (ident "reverse") (findTopLevelDeclsOf "r" modul) @?= #{withReverse}
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

toCode :: SolutionConfig -> [String] -> String
toCode config programs = intercalate "\n----\n" $ configText : programs
  where
    configText = BS.unpack . encode $ toSolutionConfigOpt config

gradeIO :: String -> String -> IO String
gradeIO task submission = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "Grade-test" $ \dir -> do
    setCurrentDirectory dir
    grade execWriterT (throwM . CustomException) (tell . show) dir task submission

hlintIO :: SolutionConfig -> String -> Bool -> IO [Either String String]
hlintIO config content asError = do
  tmp <- liftIO getTemporaryDirectory
  withTempDirectory tmp "Template-test" $ \dir -> do
    setCurrentDirectory dir
    let file = dir <> "Main.hs"
    writeFile file content
    feedback <- getHlintFeedback display config file asError
    return $ (repackStrings +++ repackStrings) <$> feedback
  where
    display = if asError then errorP else infoP
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
