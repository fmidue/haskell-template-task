{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
module Haskell.Template.Task (
  FSolutionConfig (..),
  SolutionConfig,
  check,
  defaultCode,
  defaultSolutionConfig,
  finaliseConfigs,
  getCodeWorldButtonOption,
  getHlintFeedback,
  grade,
  matchTemplate,
  maybeSampleSolution,
  parse,
  rejectHint,
  rejectMatch,
  toSolutionConfigOpt,
  unsafeTemplateSegment,
  withTempDirectoryConcurrently,
  ) where

import qualified Control.Exception.Base           as MC
import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.Exts            as E
import qualified Language.Haskell.Exts.Parser     as P
import qualified System.IO                        as IO {- required to avoid encoding problems -}
import qualified Data.String.Interpolate          as SI (i, iii, iii'E)

import Haskell.Template.FileContents    (testHelperContents, testHarnessContents)
import Haskell.Template.Match
  (Location (..), Result (..), What (..), Where (..), highlight_ssi, test)

import Control.Applicative              ((<|>))
import Control.Monad                    (forM, guard, msum, unless, void, when)
import Control.Monad.Extra              (whenJust)
import Control.Monad.IO.Class           (MonadIO)
import Data.Char                        (isUpper)
import Data.Functor.Identity            (Identity (..))
import Data.List
  (delete, elemIndex, groupBy, intercalate, isInfixOf, isPrefixOf,
   singleton,
   union,
   )
import Data.List.Extra
  (genericTake, nubOrd, replace, takeEnd, takeWhileEnd)
import Data.Maybe                       (fromMaybe)
import Data.Text.Lazy                   (pack)
import Data.Typeable                    (Typeable)
import Data.Yaml
  (FromJSON, ParseException, ToJSON, decodeEither')
import Data.Yaml.Pretty
  (defConfig, encodePretty, setConfCompare)
import GHC.Generics                     (Generic (..))
import Language.Haskell.HLint           (hlint)
import Language.Haskell.Interpreter
  (GhcError (..), InterpreterError (..), MonadInterpreter, OptionVal (..),
   as, installedModulesInScope, interpret, languageExtensions, liftIO,
   loadModules, reset, runInterpreter, set, setImports, setTopLevelModules, searchPath)
import Language.Haskell.Interpreter.Unsafe
  (unsafeRunInterpreterWithArgs)
import Numeric.Natural                  (Natural)
import System.Directory (
  doesDirectoryExist,
  removePathForcibly,
  makeAbsolute,
  )
import System.FilePath (
  (<.>),
  (</>),
  pathSeparator,
  takeBaseName,
  takeExtension,
  )
import System.IO.Temp                   (createTempDirectory)
import Test.HUnit                       (Counts (..))
import Text.PrettyPrint.Leijen.Text
  (Doc, nest, text, vcat)
import Text.Read                        (readMaybe)
import Text.Regex.PCRE.Heavy            (re, sub)

{-|
Create and use a temporary directory, passing its absolute path to the
provided action.
Deleting the temporary directory happens even if the provided action throws
an Exception.
-}
withTempDirectoryConcurrently :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectoryConcurrently targetDir template process =
  MC.bracket
    (liftIO $ createTempDirectory targetDir template)
    (liftIO . removePathForcibly)
    (\x -> untilM (doesDirectoryExist x) $ makeAbsolute x >>= process)
  where
    untilM :: IO Bool -> IO a -> IO a
    untilM f g = do
      ready <- f
      if ready
        then g
        else untilM f g

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
\# configGhcLimit              - caps amount of GHC warnings/errors to display
\# configGhcErrors             - GHC warnings to enforce
\# configGhcWarnings           - GHC warnings to provide as hints
\# configHlintSuggestionsLimit - caps amount of hlint suggestions to display
\# configHlintErrors           - hlint hints to enforce, only first one encountered is displayed
\# configHlintGroups           - hlint extra hint groups to use
\# configHlintRules            - hlint extra hint rules to use
\# configHlintSuggestions      - hlint hints to provide as suggestions
\# configLanguageExtensions    - this sets LanguageExtensions for hlint as well
\# configModules               - DEPRECATED (will be ignored)
\# syntaxCutoff                - determines the last step in the syntax phase (later steps are considered semantics)
\#                               possible values (and also the order of steps):
\#                                 Compilation, GhcErrors, HlintErrors, TemplateMatch, TestSuite
\#                               default on omission is TemplateMatch; steps after TestSuite are (in this order):
\#                                 GhcWarnings, HlintSuggestions
\# disableSemantics            - will prevent the semantics phase (as determined by syntaxCutoff) from running
\#                               this means a submission will be accepted after passing the syntax phase
\# provideSampleSolution       - display provided sample solution to students after semantics feedback
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

data FeedbackPhase
  = Compilation
  | GhcErrors
  | HlintErrors
  | TemplateMatch
  | TestSuite
  deriving (Enum, Generic, Show, FromJSON, ToJSON)

data FSolutionConfig m = SolutionConfig {
    allowAdding                 :: m Bool,
    allowModifying              :: m Bool,
    allowRemoving               :: m Bool,
    addCodeWorldButton          :: m Bool,
    configGhcLimit              :: m (Maybe Natural),
    configGhcErrors             :: m [String],
    configGhcWarnings           :: m [String],
    configHlintSuggestionsLimit :: m (Maybe Natural),
    configHlintErrors           :: m [String],
    configHlintGroups           :: m [String],
    configHlintRules            :: m [String],
    configHlintSuggestions      :: m [String],
    configLanguageExtensions    :: m [String],
    configModules               :: m [String],
    provideSampleSolution       :: m Bool,
    messageOnCloningSampleSolution :: m (Maybe String),
    disableSemantics            :: m Bool,
    syntaxCutoff                :: m FeedbackPhase
  } deriving Generic
{-# DEPRECATED configModules "config Modules will be removed" #-}

type SolutionConfigOpt = FSolutionConfig Maybe

deriving instance Show SolutionConfigOpt
deriving instance FromJSON SolutionConfigOpt
deriving instance ToJSON SolutionConfigOpt

type SolutionConfig  = FSolutionConfig Identity

deriving instance Show SolutionConfig

defaultSolutionConfig :: SolutionConfigOpt
defaultSolutionConfig = SolutionConfig {
    allowAdding                 = Just True,
    allowModifying              = Just False,
    allowRemoving               = Just False,
    addCodeWorldButton          = Just True,
    configGhcLimit              = Just Nothing,
    configGhcErrors             = Just [],
    configGhcWarnings           = Just [],
    configHlintSuggestionsLimit = Just Nothing,
    configHlintErrors           = Just [],
    configHlintGroups           = Just [],
    configHlintRules            = Just [],
    configHlintSuggestions      = Just [],
    configLanguageExtensions    = Just ["NPlusKPatterns","ScopedTypeVariables"],
    configModules               = Nothing,
    provideSampleSolution       = Just False,
    messageOnCloningSampleSolution = Just Nothing,
    disableSemantics            = Just False,
    syntaxCutoff                = Just TemplateMatch
  }

toSolutionConfigOpt :: SolutionConfig -> SolutionConfigOpt
toSolutionConfigOpt SolutionConfig {..} = runIdentity $ SolutionConfig
  <$> fmap Just allowAdding
  <*> fmap Just allowModifying
  <*> fmap Just allowRemoving
  <*> fmap Just addCodeWorldButton
  <*> fmap Just configGhcLimit
  <*> fmap Just configGhcErrors
  <*> fmap Just configGhcWarnings
  <*> fmap Just configHlintSuggestionsLimit
  <*> fmap Just configHlintErrors
  <*> fmap Just configHlintGroups
  <*> fmap Just configHlintRules
  <*> fmap Just configHlintSuggestions
  <*> fmap Just configLanguageExtensions
  <*> fmap Just configModules
  <*> fmap Just provideSampleSolution
  <*> fmap Just messageOnCloningSampleSolution
  <*> fmap Just disableSemantics
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
      <*> fmap Identity configGhcLimit
      <*> fmap Identity configGhcErrors
      <*> fmap Identity configGhcWarnings
      <*> fmap Identity configHlintSuggestionsLimit
      <*> fmap Identity configHlintErrors
      <*> fmap Identity configHlintGroups
      <*> fmap Identity configHlintRules
      <*> fmap Identity configHlintSuggestions
      <*> fmap Identity configLanguageExtensions
      <*> fmap Identity configModules
      <*> fmap Identity provideSampleSolution
      <*> fmap Identity messageOnCloningSampleSolution
      <*> fmap Identity disableSemantics
      <*> fmap Identity syntaxCutoff
    combineConfigs x y = SolutionConfig {
        allowAdding                 = allowAdding                 x <|> allowAdding                 y,
        allowModifying              = allowModifying              x <|> allowModifying              y,
        allowRemoving               = allowRemoving               x <|> allowRemoving               y,
        addCodeWorldButton          = addCodeWorldButton          x <|> addCodeWorldButton          y,
        configGhcLimit              = configGhcLimit              x <|> configGhcLimit              y,
        configGhcErrors             = configGhcErrors             x <|> configGhcErrors             y,
        configGhcWarnings           = configGhcWarnings           x <|> configGhcWarnings           y,
        configHlintSuggestionsLimit = configHlintSuggestionsLimit x <|> configHlintSuggestionsLimit y,
        configHlintErrors           = configHlintErrors           x <|> configHlintErrors           y,
        configHlintGroups           = configHlintGroups           x <|> configHlintGroups           y,
        configHlintRules            = configHlintRules            x <|> configHlintRules            y,
        configHlintSuggestions      = configHlintSuggestions      x <|> configHlintSuggestions      y,
        configLanguageExtensions    = configLanguageExtensions    x <|> configLanguageExtensions    y,
        configModules               = Just [],
        provideSampleSolution       = provideSampleSolution       x <|> provideSampleSolution       y,
        messageOnCloningSampleSolution = messageOnCloningSampleSolution x <|> messageOnCloningSampleSolution y,
        disableSemantics            = disableSemantics            x <|> disableSemantics            y,
        syntaxCutoff                = syntaxCutoff                x <|> syntaxCutoff                y
      }
    emptyConfig = SolutionConfig {
        allowAdding                 = Nothing,
        allowRemoving               = Nothing,
        allowModifying              = Nothing,
        addCodeWorldButton          = Nothing,
        configGhcLimit              = Nothing,
        configGhcErrors             = Nothing,
        configGhcWarnings           = Nothing,
        configHlintSuggestionsLimit = Nothing,
        configHlintErrors           = Nothing,
        configHlintGroups           = Nothing,
        configHlintRules            = Nothing,
        configHlintSuggestions      = Nothing,
        configLanguageExtensions    = Nothing,
        configModules               = Nothing,
        provideSampleSolution       = Nothing,
        messageOnCloningSampleSolution = Nothing,
        disableSemantics            = Nothing,
        syntaxCutoff                = Nothing
      }

string :: String -> Doc
string = text . pack

check
  :: MonadIO m
  => (forall a. Doc -> m a)
  -> (Doc -> m ())
  -> FilePath
  -> String
  -> m ()
check reject inform path i = do
  checkUnsafe reject i
  (config, exts, (m,s), ms) <- processConfig reject inform i
  checkUniqueness (m : map fst ms)
  inform $ string $ "Parsing template module " <> m
  void $ parse reject exts s
  void $ parseModule exts `mapM` ms
  let mSampleSolution = lookup "SampleSolution" ms
  -- This step is currently optional and will not run if no sample solution is provided
  case mSampleSolution of
    Nothing -> do
      when (runIdentity $ provideSampleSolution config) $
        reject "'provideSampleSolution' is set, but there is no sample solution in the config."
      whenJust (runIdentity $ messageOnCloningSampleSolution config) $ const $
        reject "'messageOnCloningSampleSolution' is set, but there is no sample solution in the config."
    Just sampleSolution -> do
      let others = filter ((/="SampleSolution") . fst) ms
      let content = replace "module SampleSolution" ("module " ++ m) sampleSolution
      (modules, solutionFile) <- writeModules (m, content) others path
      sequence_ $ testPhases reject inform s solutionFile modules config exts content path
  where
    parseModule exts (m, s) = do
      inform $ string $ "Parsing module " <> m
      parse reject exts s
    checkUniqueness xs = when (nubOrd xs /= xs) $ reject "duplicate module name"

{- |
Extract the sample solution if one was provided, 'provideSampleSolution' is enabled
and 'disableSemantics' is not enabled.
-}
maybeSampleSolution :: String -> Maybe Doc
maybeSampleSolution task = do
  (config, modules) <- splitConfigAndModules abort task
  guard =<< provideSampleSolution config
  guard . not =<< disableSemantics config
  exts <- extensionsOf <$> addDefaults abort config
  ((taskName,_), otherModules) <- nameModules abort exts modules
  sampleSolution <- lookup "SampleSolution" otherModules
  pure $ string $ replace "SampleSolution" taskName sampleSolution
  where
    abort = const Nothing

{-|
Extract the value of the `addCodeWorldButton` option.
Defaults to `False` if not specified.
Also returns `False` in case the config cannot be read.
-}
getCodeWorldButtonOption :: String -> Bool
getCodeWorldButtonOption s = fromMaybe False mOption
  where
    mOption = splitConfigAndModules (const Nothing) s >>= addCodeWorldButton . fst

{-|
Enforces completely writing the file by flushing the output,
closing the handle and waiting for it to being closed.
-}
strictWriteFile :: FilePath -> String -> IO ()
strictWriteFile f x = IO.withFile f IO.WriteMode $ \h -> do
  IO.hPutStr h x
  IO.hFlush h
  IO.hClose h
  whileOpen h

{-|
Actively wait until the Handle is closed.
-}
whileOpen :: IO.Handle -> IO ()
whileOpen h =
  IO.hIsClosed h >>= flip unless (whileOpen h)

{-|
Generate consecutive syntax and possible semantics feedback in the context of an evaluation Monad.

This Monad is expected to provide a mechanism to prematurely end the evaluation
in case of failure.

This function returns an encapsulated Bool value if all tests pass.
It will only be `True` if the submission contains a clone of the sample solution,
the semantics phase is not disabled by 'disableSemantics' and
the task was also configured to add a custom message on clones via 'messageOnCloningSampleSolution'.
Otherwise, the value will always be `False`.
This can be used by the caller to conditionally add the sample solution
after the grading is already completed, with `maybeSampleSolution`.
-}
grade
  :: MonadIO m
  => (m () -> m ())
  -- ^ Evaluation function for the syntax phase
  -> (m () -> m ())
  -- ^ Evaluation function for the semantics phase
  -> (forall c. Doc -> m c)
  -- ^ display a message and fail
  -> (Doc -> m ())
  -- ^ display a message and continue
  -> FilePath
  -- ^ parent directory to use for file operations
  -> String
  -- ^ the task
  -> String
  -- ^ the submission
  -> m Bool
  -- ^ whether the conditions outlined in the description apply or not
grade withSyntax withSemantics reject inform dirname task submission = do
    withSyntax $ checkUnsafe reject submission
    (config, exts, (moduleName', template), others) <- processConfig
      (rejectWithMessage reject $ string informTutorMessage)
      (const $ pure ())
      task
    (modules, solutionFile) <- writeModules (moduleName', submission) others dirname
    let
     (syntax, semantics) = splitAt (fromEnum (syntaxCutoff config) + 1)
      $ testPhases reject inform template solutionFile modules config exts submission dirname
    withSyntax $ sequence_ syntax
    if runIdentity $ disableSemantics config
    then pure False
    else do
     withSemantics $ sequence_ semantics
     case
      (,) <$> lookup "SampleSolution" others
          <*> runIdentity (messageOnCloningSampleSolution config)
      of
        Nothing                       -> pure False
        Just (sampleSolution,message) -> catchSampleSolutionClone
          reject
          (withSemantics $ inform $ string message)
          exts
          (replace "SampleSolution" moduleName' sampleSolution)
          submission

rejectHint :: Doc
rejectHint = [SI.iii'E|
  Unless you fix the above,
  your submission will not be considered further
  (e.g., no tests being run on it).
  |]

extensionsOf :: SolutionConfig -> [E.Extension]
extensionsOf = fmap readAll . msum . configLanguageExtensions
  where
    readAll ('N':'o':y:ys) | isUpper y = readExtension E.DisableExtension (y : ys)
    readAll x                          = readExtension E.EnableExtension x
    readExtension :: (E.KnownExtension -> E.Extension) -> String -> E.Extension
    readExtension e x = maybe (E.UnknownExtension x) e $ readMaybe x

getHlintFeedback
  :: MonadIO m
  => (Doc -> m a)
  -> SolutionConfig
  -> FilePath
  -- ^ directory where to write @additional.yaml@ for hints to check to
  -> String
  -> Bool
  -> m [a]
getHlintFeedback documentInfo config dir file asError = case hints of
  [] -> return []
  _  -> do
    liftIO $ strictWriteFile additional $ hlintConfig rules
    feedbackIdeas <- liftIO $ hlint $ addRules $
      file
      :  fmap ("--only=" ++) hints
      ++ ["--with-group=" ++ group | group <- msum $ configHlintGroups config]
      ++ ["--language=" ++ ext | ext <- msum $ configLanguageExtensions config]
      ++ ["--quiet"]
    sequence $ hlintFeedback feedbackIdeas
  where
    addRules
      | null rules = id
      | otherwise  = (:) ("--hint=" ++ additional)
    additional = dir </> "additional.yaml"
    rules = runIdentity $ configHlintRules config
    hints = runIdentity $ selectHints config
    selectHints =
      if asError
      then configHlintErrors
      else configHlintSuggestions
    hintLimit =
      maybe id genericTake $ runIdentity $ configHlintSuggestionsLimit config
    hlintFeedback feedbackIdeas =
      hintLimit [documentInfo $ string $ editFeedback $ show comment | comment <- feedbackIdeas]

editFeedback :: String -> String
editFeedback xs = case elemIndex ':' xs of
      Just index ->
        let (path, position) = splitAt index xs
        in takeWhileEnd (/= pathSeparator) path ++ position
      Nothing    -> xs

hlintConfig :: [String] -> String
hlintConfig rules = unlines ["- " ++ r | r <- rules]

compileWithArgsAndCheck
  :: MonadIO m
  => FilePath
  -> (forall b. Doc -> m b)
  -> (Doc -> m ())
  -> SolutionConfig
  -> [String]
  -> Bool
  -> m ()
compileWithArgsAndCheck dirname reject inform config modules asError = unless (null ghcOpts) $ do
  ghcErrors <-
    liftIO $ unsafeRunInterpreterWithArgs ghcOpts (compiler dirname config modules)
  checkResult reject ghcErrors how howMany $ const $ return ()
  where
    makeOpts xs = ("-w":) $ ("-Werror=" ++) <$> xs
    ghcOpts  = makeOpts $ msum (warnings config)
    (warnings, how) =
      if asError
      then (configGhcErrors,   rejectWithHint)
      else (configGhcWarnings, inform)
    howMany = runIdentity $ configGhcLimit config
    rejectWithHint = rejectWithMessage reject rejectHint

matchTemplate
  :: Monad m
  => (forall a. Doc -> m a)
  -> SolutionConfig
  -> Int
  -> [E.Extension]
  -> String
  -> String
  -> m ()
matchTemplate reject config context exts template submission =
  runMatchTestOn reject exts template submission $ \case
    Fail loc -> mapM_ (rejectMatch rejectWithHint config context template submission) loc
      where
        rejectWithHint = rejectWithMessage reject rejectHint
    Ok _     -> return ()

catchSampleSolutionClone
  :: Monad m
  => (forall a. Doc -> m a)
  -> m ()
  -> [E.Extension]
  -> String
  -> String
  -> m Bool
catchSampleSolutionClone reject displayMessage exts sample submission =
  runMatchTestOn reject exts sample submission $ \case
    Fail loc | any missingOrDifferent loc
      -> pure False
    _ -> displayMessage >> pure True
  where
    missingOrDifferent (SrcSpanInfo _ OnlySubmission _) = False
    missingOrDifferent _                                = True

runMatchTestOn
  :: Monad m
  => (forall a. Doc -> m a)
  -> [E.Extension]
  -> String
  -> String
  -> (Result () -> m b)
  -> m b
runMatchTestOn reject exts rawTemplate rawSubmission whatToDo = do
  template  <- parse reject exts rawTemplate
  submission <- parse reject exts rawSubmission
  case test template submission of
    Continue -> reject [SI.i|Haskell.Template.Central.matchTemplate:
#{informTutorMessage}|]
    otherResult -> whatToDo otherResult

deriving instance Typeable Counts

handleCounts
  :: MonadIO m
  => (forall a. Doc -> m a)
  -> (Doc -> m ())
  -> IO (Counts, String -> String)
  -> m ()
handleCounts reject inform runResult = do
  result <- liftIO runResult
  case result of
    (Counts {errors=x, failures=0}, f) | x /= 0 -> do
      inform "Some error occurred before fully testing the solution:"
      reject (string (f ""))
      -- e.g. quickcheck timeout errors
    (Counts {errors=0, failures=0}, _) -> pure ()
    (_                            , f) -> reject (string (f ""))

checkResult
  :: Monad m
  => (forall b. Doc -> m b)
  -> Either InterpreterError a
  -> (Doc -> m ())
  -> Maybe Natural
  -> (a -> m ())
  -> m ()
checkResult reject result handleError mErrorLimit handleResult = case result of
  Right result' -> handleResult result'
  Left (WontCompile msgs) -> handleError $ string
    $ intercalate "\n" $ amount
      $ map (editFeedback . formatHyperlinks) $ filterWerrors msgs
  Left err -> reject $
    vcat ["An unexpected error occurred.",
          "This is usually not caused by a fault within your solution.",
          "Please contact your lecturers, providing the following error message:",
          nest 4 $ string $ show err]
  where
    amount = maybe id genericTake mErrorLimit
    filterWerrors xs = nubOrd
      -- 'nubOrd' is used only because hint provides duplicates for each error:
      -- issue filed at: https://github.com/haskell-hint/hint/issues/83
      [x | GhcError x <- xs
         , x /= "<no location info>: error: \nFailing due to -Werror."]
    -- This fixes the broken formatting of terminal hyperlinks in an error message
    formatHyperlinks = sub
      [re|(?x)
        \[
        \x1b]8;;
        (https?://[\w\.-]+(?:/[\w-]*)*/?)
        \x1b\\
        [\w-]*
        \x1b]8;;
        \x1b\\
        \]
      |]
      (\case
          -- only keep the capture group (valid link) and discard rest of the match
          (link:_) -> "[" ++ link ++ "]";
          []       -> []
      )

interpreter
  :: MonadInterpreter m
  => FilePath
  -> SolutionConfig
  -> [String]
  -> m (IO (Counts, ShowS))
interpreter dirname config modules = do
  prepareInterpreter dirname config modules
  interpret "TestHarness.run Test.test" (as :: IO (Counts, ShowS))

compiler :: MonadInterpreter m => FilePath -> SolutionConfig -> [String] -> m Bool
compiler dirname config modules = do
  prepareInterpreter dirname config modules
  interpret "Prelude.True" (as :: Bool)

prepareInterpreter :: MonadInterpreter m => FilePath -> SolutionConfig -> [String] -> m ()
prepareInterpreter dirname config modules = do
  set [languageExtensions := map read (msum $ configLanguageExtensions config)]
  reset -- Make sure nothing is available
  set [installedModulesInScope := False]
  set [searchPath := [dirname]]
  loadModules ("TestHarness" : modules)
  setTopLevelModules modules
  setImports ["Prelude", "Test.HUnit", "TestHarness"]

parse
  :: Monad m
  => (forall a. Doc -> m a)
  -> [E.Extension]
  -> String
  -> m (E.Module E.SrcSpanInfo)
parse reject' exts' m = case E.readExtensions m of
  Nothing -> reject' "cannot parse LANGUAGE pragmas at top of file"
  Just (_, exts) ->
    let parseMode = P.defaultParseMode
                    { P.extensions = exts ++ exts' }
    in case P.parseModuleWithMode parseMode m of
         P.ParseOk a -> return a
         P.ParseFailed loc msg ->
           rejectParse reject' m loc msg

rejectParse :: (Doc -> t) -> String -> E.SrcLoc -> String -> t
rejectParse reject' m loc msg =
  let (lPre, _) = splitAt (E.srcLine loc) $ lines m
      lPre'     = takeEnd 3 lPre
      tag       = replicate (E.srcColumn loc - 1) '.' ++ "^"
  in reject' $ vcat
       ["Syntax error (your solution is no Haskell program):",
        bloc $ lPre' ++ [tag],
        string msg]

rejectMatch
  :: Applicative m
  => (forall a. Doc -> m a)
  -> SolutionConfig
  -> Int
  -> String
  -> String
  -> Location
  -> m ()
rejectMatch reject config context i b l = case l of
  SrcSpanInfoPair w sp1 sp2 ->
    unless (allowedOperation w allowModifying) $ reject $ vcat
      ["Your solution does not fit the template:" , "",
       "Template:"   , bloc $ highlight_ssi sp1 context i,
       "Submission:" , bloc $ highlight_ssi sp2 context b]
  SrcSpanInfo w OnlyTemplate sp ->
    unless (allowedOperation w allowRemoving) $ reject $ vcat
      ["Missing within your submission:",
       "Template:",
       bloc $ highlight_ssi sp context i]
  SrcSpanInfo w OnlySubmission sp ->
    unless (allowedOperation w allowAdding) $ reject $ vcat
      ["Only within your submission (but not within the template):",
       bloc $ highlight_ssi sp context b]
  where
    allowedOperation what conf = what `notElem` preventChangeTo
      && runIdentity (conf config)
    preventChangeTo = [CompleteModule, HeadOfModule, ModuleImport, Pragma]

bloc :: [String] -> Doc
bloc codeLines =
  let dash = string $ '+' : replicate 30 '-'
  in  vcat [ dash, vcat $ map (string . ("| " ++)) codeLines, dash ]

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

addDefaults :: Monad m => (forall a. Doc -> m a) -> SolutionConfigOpt -> m SolutionConfig
addDefaults reject f = maybe
  (reject "There is a required configuration parameter missing")
  return
  $ finaliseConfigs [f, defaultSolutionConfig]

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

unsafeTemplateSegment :: String -> String
unsafeTemplateSegment task = either id id $ do
  let (config, modules) = fromMaybe (defaultSolutionConfig, []) $
        splitConfigAndModules (const Nothing) task
      exts = maybe [] extensionsOf $ addDefaults (const Nothing) config
  snd . fst <$> nameModules Left exts modules

nameModules
  :: Monad m
  => (forall a. String -> m a)
  -> [E.Extension]
  -> [String]
  -> m ((String, String), [(String, String)])
nameModules reject exts modules =
  case withNames exts modules of
    P.ParseFailed _   msg  ->
      reject $ "Please contact a tutor sending the following error report:\n" <> msg
    P.ParseOk [] -> reject "No modules"
    P.ParseOk (m:ms) -> return (m,ms)

withNames :: [E.Extension] -> [String] -> P.ParseResult [(String, String)]
withNames exts mods =
  (`zip` mods) <$> mapM (fmap moduleName . E.parseFileContentsWithExts exts) mods

moduleName :: E.Module l -> String
moduleName (E.Module _ (Just (E.ModuleHead _ (E.ModuleName _ n) _ _)) _ _ _) = n
moduleName (E.Module _ Nothing _ _ _) = "Main"
moduleName _                          = error "unsupported module type"

processConfig
  :: Monad m
  => (forall a. Doc -> m a)
  -- ^ display a message and fail
  -> (Doc -> m ())
  -- ^ display a message and continue
  -> String
  -- ^ raw configuration
  -> m (FSolutionConfig Identity, [E.Extension], (String,String), [(String,String)])
processConfig reject inform rawConfig = do
  (config, modules) <- splitConfigAndModules reject rawConfig
  inform $ string $ "Parsed the following setting options:\n" ++ show config
  completedConfig <- addDefaults reject config
  inform $ string $ "Completed configuration to:\n" ++ show completedConfig
  let exts = extensionsOf completedConfig
  ((m,s), ms) <- nameModules (reject . string) exts modules
  return (completedConfig, exts, (m,s), ms)

checkUnsafe :: Monad m => (forall a. Doc -> m a) -> String -> m ()
checkUnsafe reject rawFile =  do
  when ("System.IO.Unsafe" `isInfixOf` rawFile)
    $ reject "wants to use System.IO.Unsafe"
  when ("unsafePerformIO"  `isInfixOf` rawFile)
    $ reject "wants to use unsafePerformIO"

informTutorMessage :: String
informTutorMessage =
  [SI.i|Please inform a tutor about this issue providing your solution and this message.|]

rejectWithMessage :: (forall a. Doc -> m a) -> Doc -> Doc -> m b
rejectWithMessage reject m = reject . vcat . (: singleton m)

writeModules
  :: MonadIO m
  => (FilePath, String)
  -> [(FilePath, String)]
  -> [Char]
  -> m ([String], String)
writeModules (moduleName', submission) others dirname = do
  files <- liftIO $ ((moduleName', submission) : others)
    `forM` \(mName, contents) -> do
    let fname = dirname </> mName <.> "hs"
    strictWriteFile fname contents
    return fname
  let existingModules = map takeBaseName
        $ filter ((".hs" ==) . takeExtension)
        $ filter (`notElem` [".",".."]) files
      modules = ["Test"] `union` existingModules
      solutionFile = dirname </> (moduleName' <.> "hs")
  liftIO $ do
    unless ("Test" `elem` existingModules) $
      strictWriteFile (dirname </> "Test" <.> "hs") $ testModule moduleName'
    strictWriteFile (dirname </> "TestHelper" <.> "hs") testHelperContents
    strictWriteFile (dirname </> "TestHarness" <.> "hs")
      $ testHarnessFor solutionFile
  pure (modules, solutionFile)
  where
    testHarnessFor file =
      let quoted xs = '"' : xs ++ "\""
      in replace (quoted "Solution.hs") (quoted file) testHarnessContents
    testModule :: String -> String
    testModule s = [SI.i|module Test (test) where
import qualified #{s} (test)
test = #{s}.test|]

testPhases
  :: MonadIO m
  => (forall a. Doc -> m a)
  -> (Doc -> m ())
  -> String
  -> String
  -> [String]
  -> SolutionConfig
  -> [E.Extension]
  -> String
  -> FilePath
  -> [m ()]
testPhases reject inform template solutionFile modules config exts submission dirname =
  [
    do
    -- Reject if submission does not compile with provided hidden modules,
    -- but without Test module.
    compilation <- liftIO $ runInterpreter (compiler dirname config noTest)
    checkResult reject compilation reject Nothing $ const $ return ()

    -- Reject if submission does not compile with provided hidden modules.
    -- This only runs when allowModifying is set to True in the config
    -- and displays a custom message telling students not to change type signatures.
    when (runIdentity $ allowModifying config) $ do
      compilationWithTests <- liftIO $ runInterpreter $
        compiler dirname config modules
      checkResult reject compilationWithTests signatureError Nothing $ const $ return ()
  ,
    -- Reject if GHC warnings configured as errors are triggered by solution.
    compileWithArgsAndCheck dirname reject undefined config noTest True
  ,
    -- Reject if HLint warnings configured as errors are triggered by solution.
    void $ getHlintFeedback rejectWithHint config dirname solutionFile True
  ,
    -- Reject on task template violations according to settings (modifying, adding, deleting).
    matchTemplate reject config 2 exts template submission
  ,
    do
    -- Reject if test suite fails for submission.
    result <- liftIO $ runInterpreter (interpreter dirname config modules)
    checkResult reject result reject Nothing $ handleCounts reject inform
  ,
    do
    -- Displays GHC warnings configured as non-errors triggered by submission.
    compileWithArgsAndCheck dirname reject inform config noTest False

    -- Displays HLint suggestions configured as non-errors triggered by submission.
    void $ getHlintFeedback inform config dirname solutionFile False
  ]
  where
    noTest = delete "Test" modules

    rejectWithHint = rejectWithMessage reject rejectHint

    signatureError = const $ rejectWithHint $ string [SI.iii|
      Your code is not compatible with the test suite.
      Please do not change type signatures in the given code template.
      |]
