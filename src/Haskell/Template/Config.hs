{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Haskell.Template.Config (
  FSolutionConfig (..),
  HaskellConfig (..),
  SolutionConfigOpt,
  SolutionConfig,
  FeedbackPhase (..),
  defaultSolutionConfig,
  finaliseConfigs,
  toSolutionConfigOpt,
  displayHaskellConfig,
  parseHaskellConfig,
  defaultHaskellConfig
  ) where
import GHC.Generics (Generic (..))
import Data.Yaml (FromJSON, ToJSON)
import Numeric.Natural (Natural)
import Data.Functor.Identity (Identity (..))
import Control.Applicative ((<|>))
import Data.Yaml.Pretty (encodePretty, setConfCompare, defConfig)
import qualified Data.ByteString.Char8            as BS
import Data.List ( intersperse, groupBy, isPrefixOf )
import Data.Yaml.Aeson (ParseException, decodeEither')
import Text.PrettyPrint.Leijen.Text (Doc, text)
import Data.Text.Lazy (pack)
import qualified Data.String.Interpolate          as SI (i)
import Data.Either.Extra (fromRight')

data FeedbackPhase
  = CodeWidth
  | Compilation
  | GhcErrors
  | HlintErrors
  | TemplateMatch
  | TestSuite
  deriving (Enum, Eq, Generic, Show, FromJSON, ToJSON)

data FSolutionConfig m = SolutionConfig {
    allowAdding                 :: m Bool,
    allowModifying              :: m Bool,
    allowRemoving               :: m Bool,
    addCodeWorldButton          :: m Bool,
    addCodeWorldRenderButton    :: m Bool,
    addCodeWorldPartialRenderButton :: m Bool,
    configGhcLimit              :: m (Maybe Natural),
    configGhcErrors             :: m [String],
    configGhcWarnings           :: m [String],
    configHlintSuggestionsLimit :: m (Maybe Natural),
    configHlintErrors           :: m [String],
    configHlintGroups           :: m [String],
    configHlintRules            :: m [String],
    configHlintSuggestions      :: m [String],
    configLanguageExtensions    :: m [String],
    maxLineLength               :: m (Maybe Natural),
    provideSampleSolution       :: m Bool,
    messageOnCloningSampleSolution :: m (Maybe String),
    disableSemantics            :: m Bool,
    rigorousValidation          :: m Bool,
    syntaxCutoff                :: m FeedbackPhase
  } deriving Generic

type SolutionConfigOpt = FSolutionConfig Maybe

deriving instance Show SolutionConfigOpt
deriving instance FromJSON SolutionConfigOpt
deriving instance ToJSON SolutionConfigOpt

type SolutionConfig  = FSolutionConfig Identity

deriving instance Show SolutionConfig
deriving instance FromJSON SolutionConfig
deriving instance ToJSON SolutionConfig

data HaskellConfig = HaskellConfig
  { solutionConfig :: SolutionConfig
  , modules :: [String]
  } deriving Show

defaultSolutionConfig :: SolutionConfigOpt
defaultSolutionConfig = SolutionConfig {
    allowAdding                 = Just True,
    allowModifying              = Just False,
    allowRemoving               = Just False,
    addCodeWorldButton          = Just True,
    addCodeWorldRenderButton    = Just True,
    addCodeWorldPartialRenderButton = Just False,
    configGhcLimit              = Just Nothing,
    configGhcErrors             = Just [],
    configGhcWarnings           = Just [],
    configHlintSuggestionsLimit = Just Nothing,
    configHlintErrors           = Just [],
    configHlintGroups           = Just [],
    configHlintRules            = Just [],
    configHlintSuggestions      = Just [],
    configLanguageExtensions    = Just ["NPlusKPatterns","ScopedTypeVariables"],
    maxLineLength               = Just Nothing,
    provideSampleSolution       = Just False,
    messageOnCloningSampleSolution = Just Nothing,
    disableSemantics            = Just False,
    rigorousValidation          = Just False,
    syntaxCutoff                = Just TemplateMatch
  }

toSolutionConfigOpt :: SolutionConfig -> SolutionConfigOpt
toSolutionConfigOpt SolutionConfig {..} = runIdentity $ SolutionConfig
  <$> fmap Just allowAdding
  <*> fmap Just allowModifying
  <*> fmap Just allowRemoving
  <*> fmap Just addCodeWorldButton
  <*> fmap Just addCodeWorldRenderButton
  <*> fmap Just addCodeWorldPartialRenderButton
  <*> fmap Just configGhcLimit
  <*> fmap Just configGhcErrors
  <*> fmap Just configGhcWarnings
  <*> fmap Just configHlintSuggestionsLimit
  <*> fmap Just configHlintErrors
  <*> fmap Just configHlintGroups
  <*> fmap Just configHlintRules
  <*> fmap Just configHlintSuggestions
  <*> fmap Just configLanguageExtensions
  <*> fmap Just maxLineLength
  <*> fmap Just provideSampleSolution
  <*> fmap Just messageOnCloningSampleSolution
  <*> fmap Just disableSemantics
  <*> fmap Just rigorousValidation
  <*> fmap Just syntaxCutoff

finaliseConfigs :: [SolutionConfigOpt] -> Maybe SolutionConfig
finaliseConfigs = finaliseConfig . foldl combineConfigs emptyConfig
  where
    finaliseConfig :: SolutionConfigOpt -> Maybe SolutionConfig
    finaliseConfig SolutionConfig {..} = SolutionConfig
      <$> fmap Identity allowAdding
      <*> fmap Identity allowModifying
      <*> fmap Identity allowRemoving
      <*> fmap Identity addCodeWorldButton
      <*> fmap Identity addCodeWorldRenderButton
      <*> fmap Identity addCodeWorldPartialRenderButton
      <*> fmap Identity configGhcLimit
      <*> fmap Identity configGhcErrors
      <*> fmap Identity configGhcWarnings
      <*> fmap Identity configHlintSuggestionsLimit
      <*> fmap Identity configHlintErrors
      <*> fmap Identity configHlintGroups
      <*> fmap Identity configHlintRules
      <*> fmap Identity configHlintSuggestions
      <*> fmap Identity configLanguageExtensions
      <*> fmap Identity maxLineLength
      <*> fmap Identity provideSampleSolution
      <*> fmap Identity messageOnCloningSampleSolution
      <*> fmap Identity disableSemantics
      <*> fmap Identity rigorousValidation
      <*> fmap Identity syntaxCutoff
    combineConfigs x y = SolutionConfig {
        allowAdding                 = allowAdding                 x <|> allowAdding                 y,
        allowModifying              = allowModifying              x <|> allowModifying              y,
        allowRemoving               = allowRemoving               x <|> allowRemoving               y,
        addCodeWorldButton          = addCodeWorldButton          x <|> addCodeWorldButton          y,
        addCodeWorldRenderButton    = addCodeWorldRenderButton    x <|> addCodeWorldRenderButton    y,
        addCodeWorldPartialRenderButton = addCodeWorldPartialRenderButton x <|> addCodeWorldPartialRenderButton y,
        configGhcLimit              = configGhcLimit              x <|> configGhcLimit              y,
        configGhcErrors             = configGhcErrors             x <|> configGhcErrors             y,
        configGhcWarnings           = configGhcWarnings           x <|> configGhcWarnings           y,
        configHlintSuggestionsLimit = configHlintSuggestionsLimit x <|> configHlintSuggestionsLimit y,
        configHlintErrors           = configHlintErrors           x <|> configHlintErrors           y,
        configHlintGroups           = configHlintGroups           x <|> configHlintGroups           y,
        configHlintRules            = configHlintRules            x <|> configHlintRules            y,
        configHlintSuggestions      = configHlintSuggestions      x <|> configHlintSuggestions      y,
        configLanguageExtensions    = configLanguageExtensions    x <|> configLanguageExtensions    y,
        maxLineLength               = maxLineLength               x <|> maxLineLength               y,
        provideSampleSolution       = provideSampleSolution       x <|> provideSampleSolution       y,
        messageOnCloningSampleSolution = messageOnCloningSampleSolution x <|> messageOnCloningSampleSolution y,
        disableSemantics            = disableSemantics            x <|> disableSemantics            y,
        rigorousValidation          = rigorousValidation          x <|> rigorousValidation          y,
        syntaxCutoff                = syntaxCutoff                x <|> syntaxCutoff                y
      }
    emptyConfig = SolutionConfig {
        allowAdding                 = Nothing,
        allowRemoving               = Nothing,
        allowModifying              = Nothing,
        addCodeWorldButton          = Nothing,
        addCodeWorldRenderButton    = Nothing,
        addCodeWorldPartialRenderButton = Nothing,
        configGhcLimit              = Nothing,
        configGhcErrors             = Nothing,
        configGhcWarnings           = Nothing,
        configHlintSuggestionsLimit = Nothing,
        configHlintErrors           = Nothing,
        configHlintGroups           = Nothing,
        configHlintRules            = Nothing,
        configHlintSuggestions      = Nothing,
        configLanguageExtensions    = Nothing,
        maxLineLength               = Nothing,
        provideSampleSolution       = Nothing,
        messageOnCloningSampleSolution = Nothing,
        disableSemantics            = Nothing,
        rigorousValidation          = Nothing,
        syntaxCutoff                = Nothing
      }

displayHaskellConfig :: HaskellConfig -> String
displayHaskellConfig HaskellConfig{..} = unlines $
  BS.unpack (encode solutionConfig) : if null modules then [] else "----------" : intersperse "----------" modules

parseHaskellConfig :: String -> Either Doc HaskellConfig
parseHaskellConfig raw = do
  (solConfig, modules') <- splitConfigAndModules Left raw
  completedConfig <- addDefaults Left solConfig
  return $ HaskellConfig
    { solutionConfig = completedConfig
    , modules = modules'
    }

addDefaults :: Monad m => (forall a. Doc -> m a) -> SolutionConfigOpt -> m SolutionConfig
addDefaults reject f = maybe
  (reject "There is a required configuration parameter missing")
  return
  $ finaliseConfigs [f, defaultSolutionConfig]

splitConfigAndModules
  :: Monad m
  => (forall a. Doc -> m a)
  -> String -> m (SolutionConfigOpt, [String])
splitConfigAndModules reject configAndModules =
  either (reject . string . ("Error while parsing config:\n" <>) . show)
         (return . (,rawModules))
         eConfig
  where
    configJson:rawModules = splitModules False configAndModules
    eConfig :: Either ParseException SolutionConfigOpt
    eConfig = decodeEither' $ BS.pack configJson

splitModules :: Bool -> String -> [String]
splitModules dropFirst = map unlines
  . (if dropFirst then drop 1 else id)
  . splitBy (isPrefixOf "---")
  . lines

splitBy :: (t -> Bool) -> [t] -> [[t]]
splitBy p = dropOdd . groupBy (\l r -> not (p l) && not (p r))
  where
   dropOdd [] = []
   dropOdd [x] = [x]
   dropOdd (x:_:xs) = x:dropOdd xs

string :: String -> Doc
string = text . pack

encode :: ToJSON a => a -> BS.ByteString
encode = encodePretty $ setConfCompare compare defConfig

defaultCode :: String
defaultCode = BS.unpack (encode defaultSolutionConfig) ++
  [SI.i|\#\#\#\#\# parameter description:
\# allowAdding                 - allow adding program parts
\# allowModifying              - allow modifying program parts
\# allowRemoving               - allow removing program parts
\# addCodeWorldButton          - adds a button to transfer student visible code
\#                               into the CodeWorld editor
\# addCodeWorldRenderButton    - adds a button to transfer student visible code
\#                               into the CodeWorld runner
\# addCodeWorldPartialRenderButton - adds a button to transfer student visible
\#                                   code into the CodeWorld runner with
\#                                   preview for code containing 'undefined'
\# configGhcLimit              - caps amount of GHC warnings/errors to display
\# configGhcErrors             - GHC warnings to enforce
\# configGhcWarnings           - GHC warnings to provide as hints
\# configHlintSuggestionsLimit - caps amount of hlint suggestions to display
\# configHlintErrors           - hlint hints to enforce, only first one encountered is displayed
\# configHlintGroups           - hlint extra hint groups to use
\# configHlintRules            - hlint extra hint rules to use
\# configHlintSuggestions      - hlint hints to provide as suggestions
\# configLanguageExtensions    - this sets LanguageExtensions for hlint as well
\# maxLineLength               - submissions with lines longer than this value are rejected
\# syntaxCutoff                - determines the last step in the syntax phase (later steps are considered semantics);
\#                               possible values (and also the order of steps):
\#                                 CodeWidth, Compilation, GhcErrors, HlintErrors, TemplateMatch, TestSuite
\#                               default on omission is TemplateMatch; steps after TestSuite are (in this order):
\#                                 GhcWarnings, HlintSuggestions
\# disableSemantics            - will prevent the semantics phase (as determined by syntaxCutoff) from running;
\#                               this means a submission will be accepted after passing the syntax phase
\# provideSampleSolution       - display provided sample solution to students after semantics feedback
\# rigorousValidation          - will run all tests configured for submissions on the provided sample solution
\#                               (no effect if there is none);
\#                               this should be set while configuring the task and disabled after,
\#                               in order to reduce wait times for students
\# messageOnCloningSampleSolution - compare provided sample solution with submission and output
\#                                  this message as feedback if the submission contains the sample solution
\#                                  (provideSampleSolution will be ignored if the submission is a clone)
----------
module Solution where
import Prelude

r :: [a] -> [a]
r = undefined

----------
{- You can add additional modules separated by lines of three or more dashes: -}
{-\# LANGUAGE ScopedTypeVariables \#-}
module Test (test) where
import Prelude
{-
If this module is present, Test.test is used to check the submission.
Otherwise, Solution.test is used.

'test' has to be Test.HUnit.Testable, so assertions build with (@?=) will work,
as do plain 'Bool's.
If your test suite comprises more than a single assertion, you should use a list
of named test cases (see (~:)) to provide better feedback.

Example:
-}
import TestHelper (qc)
import TestHarness
import Test.HUnit (Test, (@?=), (~:))

import qualified Solution

test :: [Test]
test =
  ["Test with QuickCheck (random input)" ~:
     qc 5000 $ \\(xs :: [Int]) ->
       Solution.r xs == Prelude.reverse xs
  ]
----------
module SampleSolution where
import Prelude

{-
This module may provide a sample solution.
Including it is currently optional, but strongly encouraged,
as the sample will be validated the same way a student's submission would,
thus preventing a broken configuration or impossible task.
-}

r :: [a] -> [a]
r = reverse

----------
module SomeHiddenModule where
import Prelude
{- This module is also not shown to the student but is available to the code -}
{-
Also available are the following modules:

  TestHelper   (Import this in Solution or Test)
    (Use either of the following instead of 'quickCheck' to turn a property into a HUnit assertion.)
    qcWithArgs :: Testable prop => Int -> Args -> prop -> Assertion
      (Provide a timeout (in ms) and Arbitrary QuickCheck Args)
    qc'        :: Testable prop => Int -> Int -> prop -> Assertion
      (Provide a timeout (in ms) and a number for 'maxSuccess')
    qc         :: Testable prop => Int -> prop -> Assertion
      (Provide a timeout (in ms))

  TestHarness  (Import this in Test)
    syntaxCheck :: (Module SrcSpanInfo -> Assertion) -> Assertion
    findTopLevelDeclsOf :: String -> Module SrcSpanInfo -> [Decl SrcSpanInfo]
    contains
    ident :: String -> Name SrcSpanInfo -> Bool
      (Used to implement syntax checks. Example usage: see above)

    allowFailures :: Int -> [Test] -> Assertion
      (Detailed output of correct/incorrect Tests in case of failure,
      with the option to allow a fixed number of tests to fail.)
 -}|]

defaultHaskellConfig :: HaskellConfig
defaultHaskellConfig = fromRight' $ parseHaskellConfig defaultCode
