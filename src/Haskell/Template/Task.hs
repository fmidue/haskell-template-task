{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  rejectMatch,
  unsafeTemplateSegment,
  ) where

import qualified Control.Exception.Base           as MC
import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.Exts            as E
import qualified Language.Haskell.Exts.Parser     as P
import qualified System.IO                        as IO {- required to avoid encoding problems -}
import qualified Text.RawString.QQ                as RS (r)

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
   nub, partition, union)
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
   loadModules, reset, runInterpreter, set, setImports, setTopLevelModules)
import Language.Haskell.Interpreter.Unsafe
  (unsafeRunInterpreterWithArgs)
import System.Directory
  (removeFile, setCurrentDirectory)
import System.FilePath                  ((</>), (<.>), takeBaseName, takeExtension)
import System.IO.Temp                   (createTempDirectory)
import Test.HUnit                       (Counts (..))
import Text.PrettyPrint.Leijen.Text
  (Doc, nest, text, vcat)
import Text.Read                        (readMaybe)

-- TODO: get rid of this evil hack and use System.IO.Temp.withTempdirectory again when change directory bug is found/fixed
withTempDirectory :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory targetDir template =
  MC.bracket
    (liftIO $ createTempDirectory targetDir template)
    (liftIO . setCurrentDirectory)

encode :: ToJSON a => a -> BS.ByteString
encode = encodePretty $ setConfCompare compare defConfig

defaultCode :: String
defaultCode = BS.unpack (encode defaultSolutionConfig) ++
  [RS.r|##### parameter description:
# allowAdding              - allow adding program parts
# allowModifying           - allow modifying program parts
# allowRemoving            - allow removing program parts
# configGhcErrors          - GHC warnings to enforce
# configGhcWarnings        - GHC warnings to provide as hints
# configHlintErrors        - hlint hints to enforce
# configHlintGroups        - hlint extra hint groups to use
# configHlintRules         - hlint extra hint rules to use
# configHlintSuggestions   - hlint hints to provide
# configLanguageExtensions - this sets LanguageExtensions for hlint as well
# configModules            - DEPRECATED (will be ignored)
----------
module Solution where
import Prelude

r :: [a] -> [a]
r = undefined

----------
{- You can add additional modules separated by lines of three or more dashes: -}
{-# LANGUAGE ScopedTypeVariables #-}
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
     qc 5000 $ \(xs :: [Int]) ->
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
  inform $ string $ "Parsing template module " <> m
  void $ parse reject exts s
  void $ parseModule exts `mapM` ms
  where
    parseModule exts (m, s) = do
      inform $ string $ "Parsing module " <> m
      parse reject exts s

grade
  :: MonadIO m
  => (forall b . Doc -> m b)
  -> (Doc -> m ())
  -> FilePath
  -> String
  -> String
  -> m (m ())
grade reject inform tmp task submission = do
    when ("System.IO.Unsafe" `isInfixOf` submission)
      $ void $ reject "wants to use System.IO.Unsafe"
    when ("unsafePerformIO"  `isInfixOf` submission)
      $ void $ reject "wants to use unsafePerformIO"
    (mconfig, rawModules) <- splitConfigAndModules reject task
    config                <- addDefaults reject mconfig
    let exts = extensionsOf config
    ((moduleName', template), others) <-
      nameModules (reject . string) exts rawModules
    liftIO $ withTempDirectory tmp "Template" $ \ dirname -> do
      files <- ((moduleName', submission) : others) `forM` \(mname, contents) -> do
        let fname = dirname </> mname <.> "hs"
        IO.writeFile fname contents
        return fname
      let existingModules = map takeBaseName
            $ filter ((".hs" ==) . takeExtension)
            $ filter (`notElem` [".",".."]) files
          modules = ["Test"] `union` existingModules
          solutionFile = dirname </> (moduleName' <.> "hs")
      when ("Test" `notElem` existingModules) $
        IO.writeFile (dirname </> "Test" <.> "hs") testModule
      IO.writeFile (dirname </> "TestHelper" <.> "hs") testHelperContents
      IO.writeFile (dirname </> "TestHarness" <.> "hs") testHarnessContents
      setCurrentDirectory dirname
      evaluate $ do
        let noTest = delete "Test" modules
        compilation <- liftIO $ runInterpreter (compiler config noTest)
        checkResult reject compilation reject $ const $ return ()
        compileWithArgsAndCheck reject inform config noTest True
        void $ getHlintFeedback reject inform config solutionFile True
        matchTemplate reject config 2 exts template submission
        result      <- liftIO $ runInterpreter (interpreter config modules)
        checkResult reject result reject $ handleCounts reject inform
        compileWithArgsAndCheck reject inform config noTest False
        void $ getHlintFeedback reject inform config solutionFile False
  where
    testModule = [RS.r|module Test (test) where
import qualified Solution (test)
test = Solution.test|]

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
  -> (Doc -> m a)
  -> SolutionConfig
  -> String
  -> Bool
  -> m [a]
getHlintFeedback reject inform config file asError = case hints of
  [] -> return []
  _  -> do
    liftIO $ IO.writeFile additional $ hlintConfig rules
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
    (selectHints, documentInfo) =
      if asError
      then (configHlintErrors, reject)
      else (configHlintSuggestions, inform)
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
  => (Doc -> m a)
  -> (Doc -> m ())
  -> SolutionConfig
  -> [String]
  -> Bool
  -> m ()
compileWithArgsAndCheck reject inform config modules asError = unless (null ghcOpts) $ do
  ghcErrors <-
    liftIO $ unsafeRunInterpreterWithArgs ghcOpts (compiler config modules)
  checkResult (void . reject) ghcErrors how $ const $ return ()
  where
    makeOpts xs = ("-w":) $ ("-Werror=" ++) <$> xs
    ghcOpts  = makeOpts $ msum (warnings config)
    (warnings, how) =
      if asError
      then (configGhcErrors,   void . reject)
      else (configGhcWarnings, inform)

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
    Fail loc -> sequence_ $ rejectMatch reject config context template submission <$> loc
    Ok _     -> return ()
    Continue -> void $ reject [RS.r|Haskell.Template.Central.matchTemplate:
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
  Left (WontCompile msgs) -> void $ handleError $ string $ intercalate "\n" $
    filterWerrors msgs
  Left err -> void $ reject $
    vcat ["An unexpected error occurred.",
          "This is usually not caused by a fault within your solution.",
          "Please contact your lecturers providing the following error message:",
          nest 4 $ string $ show err]
  where
    filterWerrors xs =
      -- 'nub' is used only because hint provides duplicates for each error:
      -- issue filed at: https://github.com/haskell-hint/hint/issues/83
      nub [x | GhcError x <- xs
             , x /= "<no location info>: error: \nFailing due to -Werror."]

interpreter
  :: MonadInterpreter m
  => SolutionConfig
  -> [String]
  -> m (Counts, ShowS)
interpreter config modules = do
  prepareInterpreter config modules
  interpret "TestHarness.run Test.test" (as :: (Counts, ShowS))

compiler :: MonadInterpreter m => SolutionConfig -> [String] -> m Bool
compiler config modules = do
  prepareInterpreter config modules
  interpret "Prelude.True" (as :: Bool)

prepareInterpreter :: MonadInterpreter m => SolutionConfig -> [String] -> m ()
prepareInterpreter config modules = do
  set [languageExtensions := map read (msum $ configLanguageExtensions config)]
  reset -- Make sure nothing is available
  set [installedModulesInScope := False]
  loadModules ("TestHarness" : modules)
  setTopLevelModules modules
  setImports $ ["Prelude", "Test.HUnit", "TestHarness"]

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
      lpre'     = reverse $ take 3 $ reverse lpre
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
    P.ParseOk ms -> case partition (("Solution" ==) . fst) ms of
      ([]  , _ ) -> case partition (("Main" ==) . fst) ms of
        ([]  , _ ) -> reject "No \"Solution\" module in config."
        (x:xs, ys) -> return (x, xs ++ ys)
      (x:xs, ys) -> return (x, xs ++ ys)

withNames :: [E.Extension] -> [String] -> P.ParseResult [(String, String)]
withNames exts mods =
  (`zip` mods) <$> mapM (fmap moduleName . E.parseFileContentsWithExts exts) mods

moduleName :: E.Module l -> String
moduleName (E.Module _ (Just (E.ModuleHead _ (E.ModuleName _ n) _ _)) _ _ _) = n
moduleName (E.Module _ Nothing _ _ _) = "Main"
moduleName _                          = error "unsopported module type"
