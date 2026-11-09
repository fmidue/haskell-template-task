{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Template.ConfigSpec where

import Control.Exception (IOException, try)
import Control.Monad.Identity (Identity (Identity))
import Data.Aeson (ToJSON)
import Data.Either (isRight)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)
import Haskell.Template.Task (
  FMaybeCaseOfOpportunityDetectionSetting (..),
  FSolutionConfig (..),
  SolutionConfigOpt,
  Status (..),
  check,
  defaultCode,
  defaultSolutionConfig,
 )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

import Data.Either.Extra (mapLeft)
import Data.Maybe (fromMaybe)
import qualified Data.String.Interpolate as SI (i)

adjustedDefaultConfig :: SolutionConfigOpt
adjustedDefaultConfig =
  defaultSolutionConfig {
    addCodeWorldRenderButton = Just False
    , configGhcErrors = Just []
    , configGhcWarnings = Just []
    , configHlintErrors = Just []
    , configHlintSuggestions = Just []
    , rigorousValidation = Just False
    , detectOpportunitiesToUseCaseOfWithMaybe = Nothing
    }

encode :: ToJSON a => a -> Maybe String -> String
encode doc override = encodedConfig ++ "\n" ++ fromMaybe "" override ++ "\n" ++ defaultCodeModules
  where
    encodedConfig = unpack $ decodeUtf8Lenient $ encodePretty (setConfCompare compare defConfig) doc
    defaultCodeModules = unlines $ dropWhile (not . isPrefixOf "---") (lines defaultCode)

evaluateCheck :: String -> IO (Either String (), [String])
evaluateCheck configString = withSystemTempDirectory "haskell-template-task" $ \dir -> do
  ref <- newIORef id

  let updateInform doc = modifyIORef ref (\is -> is . (show doc :))

  result <- try (check (fail . show) updateInform dir configString) :: IO (Either IOException ())

  infos <- readIORef ref

  pure (mapLeft show result, infos [])

spec :: Spec
spec = describe "Task config" $ do
  describe "detectOpportunitiesToUseCaseOfWithMaybe" $ do
    it "should be configured by default" $ do
      (mRejection, _) <-
        evaluateCheck
          $ flip encode Nothing
          $ adjustedDefaultConfig {
            detectOpportunitiesToUseCaseOfWithMaybe = detectOpportunitiesToUseCaseOfWithMaybe defaultSolutionConfig
            }

      mRejection `shouldSatisfy` isRight
    it "should allow to override the status while preserving other default values" $ do
      result <-
        evaluateCheck
          $ encode adjustedDefaultConfig
          $ Just
            [SI.i|
detectOpportunitiesToUseCaseOfWithMaybe:
  status: Reject
|]

      let expectedConfig = MaybeCaseOfOpportunityDetectionSetting (Identity Reject) (Identity True) (Identity Nothing)

      result `shouldSatisfy` \(mRejection, infos) ->
        isRight mRejection && any (isInfixOf (show expectedConfig)) infos
    it "should not allow to provide offerReplacement without setting status" $ do
      result <-
        evaluateCheck
          $ encode adjustedDefaultConfig
          $ Just
            [SI.i|
detectOpportunitiesToUseCaseOfWithMaybe:
  offerReplacement: false
|]
      result `shouldSatisfy` \(mRejection, _) -> case mRejection of
        Right () -> False
        Left errorMsg -> "key \\\"status\\\" not found" `isInfixOf` errorMsg
    it "should not allow to provide additionalMessage without setting status" $ do
      result <-
        evaluateCheck
          $ encode adjustedDefaultConfig
          $ Just
            [SI.i|
detectOpportunitiesToUseCaseOfWithMaybe:
  additionalMessage: "hello world"
|]
      result `shouldSatisfy` \(mRejection, _) -> case mRejection of
        Right () -> False
        Left errorMsg -> "key \\\"status\\\" not found" `isInfixOf` errorMsg
    it "should prefer explicit value over default" $ do
      result <-
        evaluateCheck
          $ encode adjustedDefaultConfig
          $ Just
            [SI.i|
detectOpportunitiesToUseCaseOfWithMaybe:
  status: Reject
  offerReplacement: false
|]

      let expectedConfig = MaybeCaseOfOpportunityDetectionSetting (Identity Reject) (Identity False) (Identity Nothing)

      result `shouldSatisfy` \(mRejection, infos) ->
        isRight mRejection && any (isInfixOf (show expectedConfig)) infos
    it "should correctly handle explicit Ignore status without other overrides" $ do
      result <-
        evaluateCheck
          $ encode adjustedDefaultConfig
          $ Just
            [SI.i|
detectOpportunitiesToUseCaseOfWithMaybe:
  status: Ignore
|]

      let expectedConfig = MaybeCaseOfOpportunityDetectionSetting (Identity Ignore) (Identity False) (Identity Nothing)

      result `shouldSatisfy` \(mRejection, infos) ->
        isRight mRejection && any (isInfixOf (show expectedConfig)) infos
    it "should not allow to set offerReplacement to true when status is set to Ignore" $ do
      result <-
        evaluateCheck
          $ encode adjustedDefaultConfig
          $ Just
            [SI.i|
detectOpportunitiesToUseCaseOfWithMaybe:
  status: Ignore
  offerReplacement: true
|]
      result `shouldSatisfy` \(mRejection, _) -> case mRejection of
        Right () -> False
        Left errorMsg -> "Cannot set 'offerReplacement' to true when 'status' is 'Ignore'" `isInfixOf` errorMsg
    it "should not allow to provide an additionalMessage when status is set to Ignore" $ do
      result <-
        evaluateCheck
          $ encode adjustedDefaultConfig
          $ Just
            [SI.i|
detectOpportunitiesToUseCaseOfWithMaybe:
  status: Ignore
  additionalMessage: "hello world"
|]
      result `shouldSatisfy` \(mRejection, _) -> case mRejection of
        Right () -> False
        Left errorMsg -> "Cannot set an 'additionalMessage' when 'status' is 'Ignore'" `isInfixOf` errorMsg
    it "should reject config with misspelled field" $ do
      result <-
        evaluateCheck
          $ encode adjustedDefaultConfig
          $ Just
            [SI.i|
detectOpportunitiesToUseCaseOfWithMaybe:
  status: Hint
  aditionalMessage: "hello world"
|]
      result `shouldSatisfy` \(mRejection, _) -> case mRejection of
        Right () -> False
        Left errorMsg -> "unknown fields: [\\\"aditionalMessage\\\"]" `isInfixOf` errorMsg
