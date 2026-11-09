{-# LANGUAGE QuasiQuotes #-}

module Haskell.Template.GradingSpec where

import Control.Exception (IOException, try)
import Data.Aeson (ToJSON)
import Data.Either.Extra (isRight, mapLeft)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.String.Interpolate as SI (i)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)
import Haskell.Template.Task (
  FMaybeCaseOfOpportunityDetectionSetting (..),
  FSolutionConfig (..),
  SolutionConfigOpt,
  Status (..),
  defaultCode,
  defaultSolutionConfig,
  grade,
 )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

adjustedDefaultConfig :: SolutionConfigOpt
adjustedDefaultConfig =
  defaultSolutionConfig {
    addCodeWorldRenderButton = Just False
    , configGhcErrors = Just []
    , configGhcWarnings = Just []
    , configHlintErrors = Just []
    , configHlintSuggestions = Just []
    , rigorousValidation = Just False
    , detectOpportunitiesToUseCaseOfWithMaybe =
        Just $
          MaybeCaseOfOpportunityDetectionSetting {
            status = Just Ignore
            , offerReplacement = Just False
            , additionalMessage = Just Nothing
            }
    }

encode :: ToJSON a => a -> Maybe String -> String
encode doc override = encodedConfig ++ "\n" ++ fromMaybe "" override ++ "\n" ++ defaultCodeModules
  where
    encodedConfig = unpack $ decodeUtf8Lenient $ encodePretty (setConfCompare compare defConfig) doc
    defaultCodeModules = unlines $ dropWhile (not . isPrefixOf "---") (lines defaultCode)

evaluateGrade :: String -> String -> IO (Either String Bool, [String])
evaluateGrade configString submission = withSystemTempDirectory "haskell-template-task" $ \dir -> do
  ref <- newIORef id

  let updateInform doc = modifyIORef ref (\is -> is . (show doc :))

  result <- try (grade id id (fail . show) updateInform dir configString submission) :: IO (Either IOException Bool)

  infos <- readIORef ref

  pure (mapLeft show result, infos [])

spec :: Spec
spec = describe "Grading" $ do
  it "doesn't parse the submission when syntaxCutoff is set to 'CodeWidth' and disableSemantics is true" $ do
    result <-
      evaluateGrade
        (encode adjustedDefaultConfig $ Just $ unlines ["syntaxCutoff: CodeWidth", "disableSemantics: true"])
        [SI.i|module Main where

main ::
|]

    result `shouldSatisfy` \(mResult, _) -> isRight mResult
