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
{-# LANGUAGE NamedFieldPuns #-}
module Haskell.Template.Task (
  check,
  getCodeWorldButtonOption,
  getCodeWorldRenderButtonOption,
  getCodeWorldPartialRenderButtonOption,
  getHlintFeedback,
  grade,
  matchTemplate,
  maybeSampleSolution,
  parse,
  rejectHint,
  rejectMatch,
  initialTask
  ) where

import qualified Language.Haskell.Exts            as E
import qualified Language.Haskell.Exts.Parser     as P
import qualified System.IO                        as IO {- required to avoid encoding problems -}
import qualified Data.String.Interpolate          as SI (i, iii)

import Haskell.Template.FileContents    (testHelperContents, testHarnessContents)
import Haskell.Template.Match
  (Location (..), Result (..), What (..), Where (..), highlight_ssi)
import qualified Haskell.Template.Match as Match (test)

import Control.Monad                    (forM, guard, msum, unless, void, when)
import Control.Monad.Extra              ((&&^), whenJust)
import Control.Monad.IO.Class           (MonadIO)
import Data.Char                        (isUpper)
import Data.Functor.Identity            (Identity (..))
import Data.List
  (delete, elemIndex, intercalate, isInfixOf,
   union,
   )
import Data.List.Extra
  (genericTake, nubOrd, replace, takeEnd, takeWhileEnd)
import Data.Maybe                       (fromMaybe)
import Data.Text.Lazy                   (pack)
import Data.Typeable                    (Typeable)
import Language.Haskell.HLint           (hlint)
import Language.Haskell.Interpreter
  (GhcError (..), InterpreterError (..), MonadInterpreter, OptionVal (..),
   Extension(UnknownExtension), as, installedModulesInScope, interpret, languageExtensions, liftIO,
   loadModules, reset, runInterpreter, set, setImports, searchPath)
import Language.Haskell.Interpreter.Unsafe
  (unsafeRunInterpreterWithArgs)
import Numeric.Natural                  (Natural)
import System.FilePath (
  (<.>),
  (</>),
  pathSeparator,
  takeBaseName,
  takeExtension,
  )
import Test.HUnit                       (Counts (..))
import Text.PrettyPrint.Leijen.Text
  (Doc, (<+>), empty, int, linebreak, nest, punctuate, text, vcat)
import Text.Read                        (readMaybe)
import Text.Regex.PCRE.Heavy            (re, sub)
import Haskell.Template.Config (HaskellConfig (..), SolutionConfig, FSolutionConfig (..), FeedbackPhase (..))



string :: String -> Doc
string = text . pack

check
  :: MonadIO m
  => (forall a. Doc -> m a)
  -> (Doc -> m ())
  -> FilePath
  -> HaskellConfig
  -> m ()
check reject inform path HaskellConfig{solutionConfig = config@SolutionConfig{..},..} = do
  checkUnsafe reject $ unlines modules
  let exts = extensionsOf config
  ((m,s), ms) <- nameModules (reject . string) exts modules
  checkUniqueness (m : map fst ms)
  inform $ string $ "Parsing template module " <> m
  void $ parse reject exts s
  mapM_ (checkLineLength reject s) $ runIdentity maxLineLength
  void $ parseModule exts `mapM` ms
  let mSampleSolution = lookup "SampleSolution" ms
  -- This step is currently optional and will not run if no sample solution is provided
  case mSampleSolution of
    Nothing -> do
      when (runIdentity provideSampleSolution) $
        reject "'provideSampleSolution' is set, but there is no sample solution in the config."
      whenJust (runIdentity messageOnCloningSampleSolution) $ const $
        reject "'messageOnCloningSampleSolution' is set, but there is no sample solution in the config."
    Just sampleSolution -> when (runIdentity rigorousValidation) $ do
      let stricterConfig = config
            { configGhcErrors = configGhcWarnings <> configGhcErrors
            , configHlintErrors = configHlintSuggestions <> configHlintErrors
            , configGhcWarnings = mempty
            , configHlintSuggestions = mempty
            , configGhcLimit = pure Nothing
            }
      let others = filter ((/="SampleSolution") . fst) ms
      let content = replace "module SampleSolution" ("module " ++ m) sampleSolution
      mapM_ (checkLineLength reject content) $ runIdentity maxLineLength
      (modules', solutionFile) <- writeModules (m, content) others path
      sequence_ $ testPhases reject inform s solutionFile modules' stricterConfig exts content path
  where
    parseModule exts (m, s) = do
      inform $ string $ "Parsing module " <> m
      parse reject exts s
    checkUniqueness xs = when (nubOrd xs /= xs) $ reject "duplicate module name"

initialTask :: HaskellConfig -> String
initialTask HaskellConfig {..} = either id id $ do
  snd . fst <$> nameModules Left (extensionsOf solutionConfig) modules


{- |
Extract the sample solution if one was provided, 'provideSampleSolution' is enabled
and 'disableSemantics' is not enabled.
-}
maybeSampleSolution :: HaskellConfig -> Maybe Doc
maybeSampleSolution HaskellConfig{solutionConfig = solConf@SolutionConfig{..},..} = do
  guard $ runIdentity $ (&&) <$> provideSampleSolution <*> fmap not disableSemantics
  let exts = extensionsOf solConf
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
getCodeWorldButtonOption :: HaskellConfig -> Bool
getCodeWorldButtonOption HaskellConfig { solutionConfig = SolutionConfig{ addCodeWorldButton }} = runIdentity addCodeWorldButton

{-|
Extract the value of the `addCodeWorldRenderButton` option.
Defaults to `False` if not specified.
Also returns `False` in case the config cannot be read.
-}
getCodeWorldRenderButtonOption :: HaskellConfig -> Bool
getCodeWorldRenderButtonOption HaskellConfig { solutionConfig = SolutionConfig{ addCodeWorldRenderButton }} = runIdentity addCodeWorldRenderButton

{-|
Extract the value of the `addCodeWorldPartialRenderButton` option.
Defaults to `False` if not specified.
Also returns `False` in case the config cannot be read.
-}
getCodeWorldPartialRenderButtonOption :: HaskellConfig -> Bool
getCodeWorldPartialRenderButtonOption HaskellConfig { solutionConfig = SolutionConfig{ addCodeWorldPartialRenderButton }} = runIdentity addCodeWorldPartialRenderButton

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
  -> HaskellConfig
  -- ^ the task configuration
  -> String
  -- ^ the submission
  -> m Bool
  -- ^ whether the conditions outlined in the description apply or not
grade withSyntax withSemantics reject inform dirname HaskellConfig{solutionConfig = solutionConfig@SolutionConfig{..},..} submission = do
    withSyntax $ checkUnsafe reject submission
    let exts = extensionsOf solutionConfig
    ((moduleName', template), others) <- nameModules (reject . string) exts modules
    withSyntax $ mapM_ (checkLineLength reject submission) $ runIdentity maxLineLength
    (modules', submissionFile) <- if runIdentity $ fmap (== CodeWidth) syntaxCutoff &&^ disableSemantics
      -- Completely skip file writing if code length is the only syntax phase action
      -- and semantics phase is disabled.
      then pure (undefined, undefined)
      else writeModules (moduleName', submission) others dirname
    let
     (syntax, semantics) = splitAt (fromEnum syntaxCutoff)
      $ testPhases reject inform template submissionFile modules' solutionConfig exts submission dirname
    withSyntax $ sequence_ syntax
    if runIdentity disableSemantics
    then pure False
    else do
     withSemantics $ sequence_ semantics
     case
      (,) <$> lookup "SampleSolution" others
          <*> runIdentity messageOnCloningSampleSolution
      of
        Nothing                       -> pure False
        Just (sampleSolution,message) -> catchSampleSolutionClone
          reject
          (inform $ string message)
          exts
          (replace "SampleSolution" moduleName' sampleSolution)
          submission

rejectHint :: Doc
rejectHint = [SI.iii|
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
  -> (SolutionConfig -> Identity [String])
  -> m [a]
getHlintFeedback documentInfo config dir file selectHints = case hints of
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
    hintLimit =
      maybe id genericTake $ runIdentity $ configHlintSuggestionsLimit config
    hlintFeedback feedbackIdeas =
      hintLimit [documentInfo $ string $ editFeedback $ show comment | comment <- feedbackIdeas]

editFeedback :: String -> String
editFeedback xs = case elemIndex ':' xs of
      Just index ->
        let (path, position) = splitAt index xs
        in takeWhileEnd (/= pathSeparator) path ++ position ++ "\n"
      Nothing    -> xs ++ "\n"

hlintConfig :: [String] -> String
hlintConfig rules = unlines ["- " ++ r | r <- rules]

compileWithArgsAndCheck
  :: MonadIO m
  => FilePath
  -> (forall b. Doc -> m b)
  -> (Doc -> m ())
  -> SolutionConfig
  -> [String]
  -> (SolutionConfig -> Identity [String])
  -> m ()
compileWithArgsAndCheck dirname reject how config modules selectWarnings = unless (null warnings) $ do
  ghcErrors <-
    liftIO $ unsafeRunInterpreterWithArgs ghcOpts (compiler dirname extensions modules)
  checkResult reject ghcErrors how howMany $ const $ return ()
  where
    warnings = msum (selectWarnings config)
    makeOpts xs = ("-w":) $ ("-Werror=" ++) <$> xs
    ghcOpts  = makeOpts warnings
    howMany = runIdentity $ configGhcLimit config
    extensions = extensionsOf config

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
    Ok ()    -> return ()

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
  case Match.test template submission of
    Continue -> reject [SI.i|Haskell.Template.Central.matchTemplate:
#{informTutorMessage}|]
    otherResult -> whatToDo otherResult

deriving instance Typeable Counts

handleCounts
  :: MonadIO m
  => (forall a. Doc -> m a)
  -> IO (Counts, String -> String)
  -> m ()
handleCounts reject runResult = do
  result <- liftIO runResult
  case result of
    (Counts {errors = 0, failures = 0}, _) -> pure ()
    (Counts {failures = 0}, f) -> do
      reject $ vcat [ "Some error(s) occurred before fully testing the submission:", empty, string (f "") ]
      -- e.g. quickcheck timeout errors
    (_, f) -> reject (string (f ""))

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
          "This is usually not caused by a fault within your submission.",
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
  -> [E.Extension]
  -> [String]
  -> m (IO (Counts, ShowS))
interpreter dirname exts modules = do
  prepareInterpreter dirname exts modules
  interpret "TestHarness.run Test.test" (as :: IO (Counts, ShowS))

compiler :: MonadInterpreter m => FilePath -> [E.Extension] -> [String] -> m Bool
compiler dirname exts modules = do
  prepareInterpreter dirname exts modules
  interpret "Prelude.True" (as :: Bool)

prepareInterpreter :: MonadInterpreter m => FilePath -> [E.Extension] -> [String] -> m ()
prepareInterpreter dirname exts modules = do
  set [languageExtensions := map (readExt . E.prettyExtension) exts]
  reset -- Make sure nothing is available
  set [installedModulesInScope := False]
  set [searchPath := [dirname]]
  -- All modules can be imported in source files
  loadModules ("TestHarness" : modules)
  -- All export items from these modules are in scope for the interpreter evaluation
  setImports $ ["Prelude", "Test.HUnit", "TestHarness"] ++ ["Test" | "Test" `elem` modules]
  where
    readExt input = fromMaybe (UnknownExtension input) $ readMaybe input

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
       ["Syntax error (your submission is no Haskell program):",
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
      ["Your submission does not fit the template:" , empty,
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

checkUnsafe :: Monad m => (forall a. Doc -> m a) -> String -> m ()
checkUnsafe reject rawFile =  do
  when ("System.IO.Unsafe" `isInfixOf` rawFile)
    $ reject "wants to use System.IO.Unsafe"
  when ("unsafePerformIO"  `isInfixOf` rawFile)
    $ reject "wants to use unsafePerformIO"

informTutorMessage :: String
informTutorMessage =
  [SI.i|Please inform a tutor about this issue providing your submission and this message.|]

rejectWithMessage :: (forall a. Doc -> m a) -> Doc -> Doc -> m b
rejectWithMessage reject m = reject . vcat . (: [empty, m])

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
      submissionFile = dirname </> (moduleName' <.> "hs")
  liftIO $ do
    unless ("Test" `elem` existingModules) $
      strictWriteFile (dirname </> "Test" <.> "hs") $ testModule moduleName'
    strictWriteFile (dirname </> "TestHelper" <.> "hs") testHelperContents
    strictWriteFile (dirname </> "TestHarness" <.> "hs")
      $ testHarnessFor submissionFile
  pure (modules, submissionFile)
  where
    testHarnessFor file =
      let quoted xs = '"' : xs ++ "\""
      in replace (quoted "Submission.hs") (quoted file) testHarnessContents
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
testPhases reject inform template submissionFile modules config exts submission dirname =
  [
    do
    -- Reject if submission does not compile with provided hidden modules,
    -- but without Test module.
    compilation <- liftIO $ unsafeRunInterpreterWithArgs
      -- disable default warnings (bleed into error report if code doesn't compile)
      ["-w"]
      (compiler dirname exts noTest)
    checkResult reject compilation reject Nothing $ const $ return ()

    -- Reject if submission does not compile with provided hidden modules.
    -- This only runs when allowModifying is set to True in the config
    -- and displays a custom message telling students not to change type signatures.
    when (runIdentity $ allowModifying config) $ do
      compilationWithTests <- liftIO $ runInterpreter $
        compiler dirname exts modules
      checkResult reject compilationWithTests signatureError Nothing $ const $ return ()
  ,
    -- Reject if GHC warnings configured as errors are triggered by submission.
    compileWithArgsAndCheck dirname reject rejectWithHint config noTest configGhcErrors
  ,
    -- Reject if HLint warnings configured as errors are triggered by submission.
    void $ getHlintFeedback rejectWithHint config dirname submissionFile configHlintErrors
  ,
    -- Reject on task template violations according to settings (modifying, adding, deleting).
    matchTemplate reject config 2 exts template submission
  ,
    do
    -- Reject if test suite fails for submission.
    result <- liftIO $ runInterpreter (interpreter dirname exts modules)
    checkResult reject result reject Nothing $ handleCounts reject
  ,
    do
    -- Displays GHC warnings configured as non-errors triggered by submission.
    compileWithArgsAndCheck dirname reject inform config noTest configGhcWarnings

    -- Displays HLint suggestions configured as non-errors triggered by submission.
    void $ getHlintFeedback inform config dirname submissionFile configHlintSuggestions
  ]
  where
    noTest = delete "Test" modules

    rejectWithHint = rejectWithMessage reject rejectHint

    signatureError = const $ rejectWithHint $ string [SI.iii|
      Your code is not compatible with the test suite.
      Please do adhere to type requirements expressed in the given code template.
      |]

checkLineLength :: Applicative m => (forall a. Doc -> m a) -> String -> Natural -> m ()
checkLineLength reject code maxLength = case hasLonger of
  [] -> pure ()
  xs -> rejectWithHint $ separated
    [ "Your submission contains overlong lines:"
    , separated xs
    , "The maximum line length allowed is" <+> string (show maxLength) <> "."
    ]
  where
    codeLines = lines code
    hasLonger =
      [ format i l lineLength
      | (i, l) <- zip [1..] codeLines
      , let lineLength = length l
      , fromIntegral lineLength > maxLength
      ]
    format i l lineLength = nest 2 $ vcat
      [ "Line" <+> int i <+> "(length" <+> int lineLength <> "):"
      , string l
      ]
    separated = vcat . punctuate linebreak
    rejectWithHint = rejectWithMessage reject rejectHint
