{-# LANGUAGE QuasiQuotes #-}
module Haskell.Template.TaskSpec where

import qualified Data.String.Interpolate          as SI (__i, i)
import qualified Text.PrettyPrint.Leijen.Text     as PP

import Haskell.Template.Task

import Control.Arrow                    ((+++))
import Control.Monad.IO.Class           (liftIO)
import Data.List                        (intercalate, isPrefixOf)
import Data.List.Extra                  (split)
import Data.Maybe                       (fromJust)
import Data.Text.Lazy                   (unpack)
import System.Directory
  (getTemporaryDirectory, setCurrentDirectory)
import System.IO.Temp                   (withTempDirectory)
import Test.Hspec

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
      gradeIO defaultCode useImport `shouldReturn` ()
    it "allows syntaxCheck" $
      gradeIO (withSyntaxCheck True) useImport `shouldReturn` ()
    it "error on failing syntaxCheck" $
      gradeIO (withSyntaxCheck False) useImport `shouldThrow` anyErrorCall
  where
    useImport = [SI.__i|
      module Solution where
      import Prelude
      r :: [a] -> [a]
      r = reverse
      |]
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

gradeIO :: String -> String -> IO ()
gradeIO task submission = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "Grade-test" $ \dir -> do
    setCurrentDirectory dir
    grade id (error . show) print dir task submission

hlintIO :: SolutionConfig -> String -> Bool -> IO [Either String String]
hlintIO config content asError = do
  tmp <- liftIO getTemporaryDirectory
  withTempDirectory tmp "Template-test" $ \dir -> do
    setCurrentDirectory dir
    let file = dir <> "Main.hs"
    writeFile file content
    feedback <- getHlintFeedback errorP infoP config file asError
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

errorP :: PP.Doc -> IO (Either String b)
errorP = return . Left . unpack . PP.displayT . PP.renderPretty 1.0 100

infoP :: PP.Doc -> IO (Either a String)
infoP = return . Right . unpack . PP.displayT . PP.renderPretty 1.0 100

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
