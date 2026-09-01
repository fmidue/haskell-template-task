{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Haskell.Template.Config (
  FSolutionConfig (..),
  HaskellConfig (..),
  SolutionConfigOpt,
  SolutionConfig,
  FeedbackPhase (..),
  defaultSolutionConfig,
  finaliseConfigs,
  toSolutionConfigOpt
  ) where
import GHC.Generics (Generic (..))
import Data.Yaml (FromJSON, ToJSON)
import Numeric.Natural (Natural)
import Data.Functor.Identity (Identity (..))
import Control.Applicative ((<|>))

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

data HaskellConfig = HaskellConfig
  { solutionConfig :: SolutionConfig
  , modules :: [String]
  }

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
