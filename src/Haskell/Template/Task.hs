{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
  getHlintFeedback,
  grade,
  matchTemplate,
  parse,
  rejectHint,
  rejectMatch,
  toSolutionConfigOpt,
  unsafeTemplateSegment,
  ) where

import qualified Control.Exception.Base           as MC
import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.Exts            as E
import qualified Language.Haskell.Exts.Parser     as P
import qualified System.IO                        as IO {- required to avoid encoding problems -}
import qualified Data.String.Interpolate          as SI (i, iii)

import Haskell.Template.FileContents    (testHelperContents, testHarnessContents)
import Haskell.Template.Match
  (Location (..), Result (..), What (..), Where (..), highlight_ssi, test)

import Control.Applicative              ((<|>))
import Control.Monad                    (forM, msum, unless, void, when)
import Control.Monad.IO.Class           (MonadIO)
import Data.Char                        (isUpper)
import Data.Functor.Identity            (Identity (..))
import Data.List
  (delete, elemIndex, groupBy, intercalate, isInfixOf, isPrefixOf,
   partition,
   singleton,
   union,
   )
import Data.List.Extra                  (nubOrd, replace, takeEnd)
import Data.Text.Lazy                   (pack)
import Data.Typeable                    (Typeable)
import Data.Yaml
  (FromJSON, ParseException, ToJSON, decodeEither')
import Data.Yaml.Pretty
  (defConfig, encodePretty, setConfCompare)
import GHC.Generics                     (Generic (..))
import Language.Haskell.HLint           (hlint)
import Control.Exception                (evaluate)
import Language.Haskell.Interpreter
  (GhcError (..), InterpreterError (..), MonadInterpreter, OptionVal (..),
   as, installedModulesInScope, interpret, languageExtensions, liftIO,
   loadModules, reset, runInterpreter, set, setImports, setTopLevelModules, searchPath)
import Language.Haskell.Interpreter.Unsafe
  (unsafeRunInterpreterWithArgs)
import System.Directory (
  doesDirectoryExist,
  removeFile,
  removePathForcibly,
  setCurrentDirectory,
  makeAbsolute,
  )
import System.FilePath                  ((</>), (<.>), takeBaseName, takeExtension)
import System.IO.Temp                   (createTempDirectory)
import Test.HUnit                       (Counts (..))
import Text.PrettyPrint.Leijen.Text
  (Doc, nest, text, vcat)
import Text.Read                        (readMaybe)

{-|
Create and use a temporarily created directory by
changing into the directory after creation and leaving it before its deletion.
Returning to the former directory and deleting the temporary directory
happen even if the provided action throws an Exception.
-}
withTempDirectory :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory targetDir template f =
  MC.bracket
    (liftIO $ createTempDirectory targetDir template)
    (liftIO . removePathForcibly)
    (\x -> untilM (doesDirectoryExist x) $ makeAbsolute x >>= f)
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
\# allowAdding              - allow adding program parts
\# allowModifying           - allow modifying program parts
\# allowRemoving            - allow removing program parts
\# configGhcErrors          - GHC warnings to enforce
\# configGhcWarnings        - GHC warnings to provide as hints
\# configHlintErrors        - hlint hints to enforce
\# configHlintGroups        - hlint extra hint groups to use
\# configHlintRules         - hlint extra hint rules to use
\# configHlintSuggestions   - hlint hints to provide
\# configLanguageExtensions - this sets LanguageExtensions for hlint as well
\# configModules            - DEPRECATED (will be ignored)
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

data FSolutionConfig m = SolutionConfig {
    allowAdding              :: m Bool,
    allowModifying           :: m Bool,
    allowRemoving            :: m Bool,
    configGhcErrors          :: m [String],
    configGhcWarnings        :: m [String],
    configHlintErrors        :: m [String],
    configHlintGroups        :: m [String],
    configHlintRules         :: m [String],
    configHlintSuggestions   :: m [String],
    configLanguageExtensions :: m [String],
    configModules            :: m [String]
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
    allowAdding              = Just True,
    allowModifying           = Just False,
    allowRemoving            = Just False,
    configGhcErrors          = Just [],
    configGhcWarnings        = Just [],
    configHlintErrors        = Just [],
    configHlintGroups        = Just [],
    configHlintRules         = Just [],
    configHlintSuggestions   = Just [],
    configLanguageExtensions = Just ["NPlusKPatterns","ScopedTypeVariables"],
    configModules            = Nothing
  }

toSolutionConfigOpt :: SolutionConfig -> SolutionConfigOpt
toSolutionConfigOpt SolutionConfig {..} = runIdentity $ SolutionConfig
  <$> fmap Just allowAdding
  <*> fmap Just allowModifying
  <*> fmap Just allowRemoving
  <*> fmap Just configGhcErrors
  <*> fmap Just configGhcWarnings
  <*> fmap Just configHlintErrors
  <*> fmap Just configHlintGroups
  <*> fmap Just configHlintRules
  <*> fmap Just configHlintSuggestions
  <*> fmap Just configLanguageExtensions
  <*> fmap Just configModules

finaliseConfigs :: [SolutionConfigOpt] -> Maybe SolutionConfig
finaliseConfigs = finaliseConfig . foldl combineConfigs emptyConfig
  where
    finaliseConfig :: SolutionConfigOpt -> Maybe SolutionConfig
    finaliseConfig SolutionConfig {..} = SolutionConfig
      <$> fmap Identity allowAdding
      <*> fmap Identity allowModifying
      <*> fmap Identity allowRemoving
      <*> fmap Identity configGhcErrors
      <*> fmap Identity configGhcWarnings
      <*> fmap Identity configHlintErrors
      <*> fmap Identity configHlintGroups
      <*> fmap Identity configHlintRules
      <*> fmap Identity configHlintSuggestions
      <*> fmap Identity configLanguageExtensions
      <*> fmap Identity configModules
    combineConfigs x y = SolutionConfig {
        allowAdding              = allowAdding              x <|> allowAdding              y,
        allowModifying           = allowModifying           x <|> allowModifying           y,
        allowRemoving            = allowRemoving            x <|> allowRemoving            y,
        configGhcErrors          = configGhcErrors          x <|> configGhcErrors          y,
        configGhcWarnings        = configGhcWarnings        x <|> configGhcWarnings        y,
        configHlintErrors        = configHlintErrors        x <|> configHlintErrors        y,
        configHlintGroups        = configHlintGroups        x <|> configHlintGroups        y,
        configHlintRules         = configHlintRules         x <|> configHlintRules         y,
        configHlintSuggestions   = configHlintSuggestions   x <|> configHlintSuggestions   y,
        configLanguageExtensions = configLanguageExtensions x <|> configLanguageExtensions y,
        configModules            = Just []
      }
    emptyConfig = SolutionConfig {
        allowAdding              = Nothing,
        allowRemoving            = Nothing,
        allowModifying           = Nothing,
        configGhcErrors          = Nothing,
        configGhcWarnings        = Nothing,
        configHlintErrors        = Nothing,
        configHlintGroups        = Nothing,
        configHlintRules         = Nothing,
        configHlintSuggestions   = Nothing,
        configLanguageExtensions = Nothing,
        configModules            = Nothing
      }

string :: String -> Doc
string = text . pack

check
  :: Monad m
  => (forall a . Doc -> m a)
  -> (Doc -> m ())
  -> String
  -> m ()
check reject inform i = do
  when ("System.IO.Unsafe" `isInfixOf` i)
    $ reject "wants to use System.IO.Unsafe"
  when ("unsafePerformIO"  `isInfixOf` i)
    $ reject "wants to use unsafePerformIO"
  (mconfig, modules) <- splitConfigAndModules reject i
  inform $ string $ "Parsed the following setting options:\n" ++ show mconfig
  config <- addDefaults reject mconfig
  inform $ string $ "Completed configuration to:\n" ++ show config
  let exts = extensionsOf config
  ((m,s), ms) <- nameModules (reject . string) exts modules
  checkUniqueness (m : map fst ms)
  inform $ string $ "Parsing template module " <> m
  void $ parse reject exts s
  void $ parseModule exts `mapM` ms
  where
    parseModule exts (m, s) = do
      inform $ string $ "Parsing module " <> m
      parse reject exts s
    checkUniqueness xs = when (nubOrd xs /= xs) $ reject "duplicate module name"
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

grade
  :: MonadIO m
  => (m () -> IO b)
  -> (forall b . Doc -> m b)
  -> (Doc -> m ())
  -> FilePath
  -> String
  -> String
  -> IO b
grade eval reject inform tmp task submission =
  withTempDirectory tmp "Template" $ \ dirname -> eval $ do
    when ("System.IO.Unsafe" `isInfixOf` submission)
      $ void $ reject "wants to use System.IO.Unsafe"
    when ("unsafePerformIO"  `isInfixOf` submission)
      $ void $ reject "wants to use unsafePerformIO"
    (mconfig, rawModules) <- splitConfigAndModules reject task
    config                <- addDefaults reject mconfig
    let exts = extensionsOf config
    ((moduleName', template), others) <-
      nameModules (reject . string) exts rawModules
    files <- liftIO $ ((moduleName', submission) : others)
      `forM` \(mname, contents) -> do
        let fname = dirname </> mname <.> "hs"
        strictWriteFile fname contents
        return fname
    let   existingModules = map takeBaseName
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
    do
        let noTest = delete "Test" modules
        compilation <- liftIO $ runInterpreter (compiler dirname config noTest)
        checkResult reject compilation reject $ const $ return ()
        compileWithArgsAndCheck dirname reject undefined config noTest True
        void $ getHlintFeedback rejectWithHint config solutionFile True
        matchTemplate reject config 2 exts template submission
        result      <- liftIO $ runInterpreter (interpreter dirname config modules)
        checkResult reject result reject $ handleCounts reject inform
        compileWithArgsAndCheck dirname reject inform config noTest False
        void $ getHlintFeedback inform config solutionFile False
  where
    testHarnessFor file =
      let quoted xs = '"' : xs ++ "\""
      in replace (quoted "Solution.hs") (quoted file) testHarnessContents
    testModule :: String -> String
    testModule s = [SI.i|module Test (test) where
import qualified #{s} (test)
test = #{s}.test|]
    rejectWithHint = reject . vcat . (: singleton rejectHint)

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
  -> String
  -> Bool
  -> m [a]
getHlintFeedback documentInfo config file asError = case hints of
  [] -> return []
  _  -> do
    liftIO $ strictWriteFile additional $ hlintConfig rules
    feedbackIdeas <- liftIO $ hlint $ addRules $
      file
      :  fmap ("--only=" ++) hints
      ++ ["--with-group=" ++ group | group <- msum $ configHlintGroups config]
      ++ ["--language=" ++ ext | ext <- msum $ configLanguageExtensions config]
      ++ ["--quiet"]
    liftIO $ removeFile additional
    sequence $ hlintFeedback feedbackIdeas
  where
    addRules
      | null rules = id
      | otherwise  = (:) ("--hint=" ++ additional)
    additional = "additional.yaml"
    rules = runIdentity $ configHlintRules config
    hints = runIdentity $ selectHints config
    selectHints =
      if asError
      then configHlintErrors
      else configHlintSuggestions
    hlintFeedback feedbackIdeas =
      [documentInfo $ string $ editFeedback $ show comment | comment <- feedbackIdeas]

editFeedback :: String -> String
editFeedback xs = case elemIndex ':' xs of
      Just index -> drop (index + 1) xs
      Nothing    -> xs

hlintConfig :: [String] -> String
hlintConfig rules = unlines ["- " ++ r | r <- rules]

compileWithArgsAndCheck
  :: MonadIO m
  => FilePath
  -> (Doc -> m a)
  -> (Doc -> m a)
  -> SolutionConfig
  -> [String]
  -> Bool
  -> m ()
compileWithArgsAndCheck dirname reject inform config modules asError = unless (null ghcOpts) $ do
  ghcErrors <-
    liftIO $ unsafeRunInterpreterWithArgs ghcOpts (compiler dirname config modules)
  checkResult reject ghcErrors how $ const $ return ()
  where
    makeOpts xs = ("-w":) $ ("-Werror=" ++) <$> xs
    ghcOpts  = makeOpts $ msum (warnings config)
    (warnings, how) =
      if asError
      then (configGhcErrors,   rejectWithHint)
      else (configGhcWarnings, inform)
    rejectWithHint = reject . vcat . (: singleton rejectHint)

matchTemplate
  :: Monad m
  => (Doc -> m (E.Module E.SrcSpanInfo))
  -> SolutionConfig
  -> Int
  -> [E.Extension]
  -> String
  -> String
  -> m ()
matchTemplate reject config context exts template submission = do
  mtemplate  <- parse reject exts template
  msubmission <- parse reject exts submission
  case test mtemplate msubmission of
    Fail loc -> mapM_ (rejectMatch rejectWithHint config context template submission) loc
      where
        rejectWithHint = reject . vcat . (: singleton rejectHint)
    Ok _     -> return ()
    Continue -> void $ reject [SI.i|Haskell.Template.Central.matchTemplate:
Please inform a tutor about this issue providing your solution and this message.|]

deriving instance Typeable Counts

handleCounts
  :: Monad m
  => (Doc -> m ())
  -> (Doc -> m ())
  -> (Counts, String -> String)
  -> m ()
handleCounts reject inform result = case result of
  (Counts {errors=a,failures=0} ,f) | a /= 0 -> do
    inform "Some error occurred before fully testing the solution:"
    reject (string (f ""))
    -- e.g. quickcheck timeout errors
  (Counts {errors=0, failures=0},_) -> return ()
  (_                            ,f) -> reject (string (f ""))

checkResult
  :: Monad m
  => (Doc -> m b)
  -> Either InterpreterError a
  -> (Doc -> m c)
  -> (a -> m d)
  -> m ()
checkResult reject result handleError handleResult = case result of
  Right result' -> void $ handleResult result'
  Left (WontCompile msgs) -> void $ handleError $ string
    $ editFeedback $ intercalate "\n" $ filterWerrors msgs
  Left err -> void $ reject $
    vcat ["An unexpected error occurred.",
          "This is usually not caused by a fault within your solution.",
          "Please contact your lecturers providing the following error message:",
          nest 4 $ string $ show err]
  where
    filterWerrors xs = nubOrd
      -- 'nubOrd' is used only because hint provides duplicates for each error:
      -- issue filed at: https://github.com/haskell-hint/hint/issues/83
      [x | GhcError x <- xs
         , x /= "<no location info>: error: \nFailing due to -Werror."]

interpreter
  :: MonadInterpreter m
  => FilePath
  -> SolutionConfig
  -> [String]
  -> m (Counts, ShowS)
interpreter dirname config modules = do
  prepareInterpreter dirname config modules
  interpret "TestHarness.run Test.test" (as :: (Counts, ShowS))

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
  => (Doc -> m (E.Module E.SrcSpanInfo))
  -> [E.Extension]
  -> String
  -> m (E.Module E.SrcSpanInfo)
parse reject' exts' m = case E.readExtensions m of
  Nothing -> reject' "cannot parse LANGUAGE pragmas at top of file"
  Just (_, exts) ->
    let pamo = P.defaultParseMode
               { P.extensions = exts ++ exts' }
    in case P.parseModuleWithMode pamo m of
         P.ParseOk a -> return a
         P.ParseFailed loc msg ->
           rejectParse reject' m loc msg

rejectParse :: (Doc -> t) -> String -> E.SrcLoc -> String -> t
rejectParse reject' m loc msg =
  let (lpre, _) = splitAt (E.srcLine loc) $ lines m
      lpre'     = takeEnd 3 lpre
      tag       = replicate (E.srcColumn loc - 1) '.' ++ "^"
  in reject' $ vcat
       ["Syntax error (your solution is no Haskell program):",
        bloc $ lpre' ++ [tag],
        string msg]

rejectMatch
  :: Applicative m
  => (Doc -> m a)
  -> SolutionConfig
  -> Int
  -> String
  -> String
  -> Location
  -> m ()
rejectMatch reject config context i b l = case l of
  SrcSpanInfoPair w sp1 sp2 ->
    unless (allowedOperation w allowModifying) $ void $ reject $ vcat
      ["Your solution does not fit the template:" , "",
       "Template:"   , bloc $ highlight_ssi sp1 context i,
       "Submission:" , bloc $ highlight_ssi sp2 context b]
  SrcSpanInfo w OnlyTemplate sp ->
    unless (allowedOperation w allowRemoving) $ void $ reject $ vcat
      ["Missing within your submission:",
       "Template:",
       bloc $ highlight_ssi sp context i]
  SrcSpanInfo w OnlySubmission sp ->
    unless (allowedOperation w allowAdding) $ void $ reject $ vcat
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
  => (Doc -> m (SolutionConfigOpt, [String]))
  -> String -> m (SolutionConfigOpt, [String])
splitConfigAndModules reject configAndModules =
  either (reject . string . ("Error while parsing config:\n" <>) . show)
         (return . (,rawModules))
         eConfig
  where
    configJson:rawModules = splitModules False configAndModules
    eConfig :: Either ParseException SolutionConfigOpt
    eConfig = decodeEither' $ BS.pack configJson

addDefaults :: Monad m => (Doc -> m SolutionConfig) -> SolutionConfigOpt -> m SolutionConfig
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
  let Just (mconfig, modules) =
        splitConfigAndModules (const $ Just (defaultSolutionConfig, [])) task
      exts = maybe [] extensionsOf $ addDefaults (const Nothing) mconfig
  snd . fst <$> nameModules Left exts modules

nameModules
  :: Monad m
  => (String -> m ((String, String), [(String, String)]))
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
moduleName _                          = error "unsopported module type"
