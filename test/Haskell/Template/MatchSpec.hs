{-# LANGUAGE QuasiQuotes #-}
module Haskell.Template.MatchSpec where

import qualified Text.PrettyPrint.Leijen.Text     as PP
import qualified Text.RawString.QQ                as RS (r)

import Haskell.Template.Task
import Haskell.Template.Match           (Result (..), test)

import Control.Monad.State.Lazy         (State, execState, modify)
import Data.Either                      (isLeft)
import Data.List                        (isInfixOf, isPrefixOf)
import Data.Maybe                       (fromJust)
import Data.Text.Lazy                   (unpack)
import Test.Hspec

withAll :: SolutionConfig
withAll = fromJust $ finaliseConfigs
  [defaultSolutionConfig {
      allowModifying = Just True,
      allowRemoving = Just True }]

withAdding :: SolutionConfig
withAdding = fromJust $ finaliseConfigs [defaultSolutionConfig]

noEdits :: SolutionConfig
noEdits = fromJust $ finaliseConfigs
  [defaultSolutionConfig { allowAdding = Just False }]

spec :: Spec
spec =
  describe "template match" $ do
    it "accepts identical code" $
      matchTemplate errorP withAdding 0 [] modUndefined modUndefined
      `shouldBe` Right ()
    it "accepts identical code without comment" $
      getComment withAdding modUndefined modUndefined
      `shouldBe` Right []
    it "rejects code where types are changed" $
      head <$> getComment withAdding modUndefinedParameter modUndefined
      `shouldBe` Right (Mismatch ["foo :: Bool -> Int"
                                 ,"       ^^^^^^^^^^^"
                                 ,"foo :: Int"
                                 ,"       ^^^"])
    it "rejects code where parameters are removed" $
      getComment withAdding modUndefinedParameter modUndefinedFunction
      `shouldBe` Right [Missing ["foo t = undefined"
                                ,"    ^^           "]]
    it "accept code where types for given functions are added" $
      getComment withAdding modNoType modUndefined
      `shouldBe` Right []
    it "accepts code where undefined is replaced without comment" $
      getComment withAdding modUndefined mod42
      `shouldBe` Right []
    it "accepts code where function definitions are reordered" $
      getComment withAdding modUndefined mod43
      `shouldBe` Right []
    it "accept code with guards" $
      getComment withAdding modUndefined modGuards
      `shouldBe` Right []
    it "accept code with guards and parameter" $
      getComment withAdding modUndefinedParameter modGuardsParameter
      `shouldBe` Right []
    it "rejects code where parts beside undefined are changed" $
      matchTemplate errorP withAdding 0 [] mod42 mod43 `shouldSatisfy` isLeft
    it "marks not matching parts within functions" $
      getComment withAdding mod42 mod43
      `shouldBe` Right [Mismatch ["foo = 42"
                                 ,"      ^^"
                                 ,"foo = 43"
                                 ,"      ^^"]]
    it "marks removes" $
      getComment withAdding modUndefined modRemove
      `shouldBe` Right [Missing ["main :: IO ()"
                                ,"^^^^^^^^^^^^^"]]
    it "rejects adding code if wanted" $
      head <$> getComment noEdits modUndefined modAdd
      `shouldBe` Right (Additional ["helper :: Int"
                                   ,"^^^^^^^^^^^^^"])
    it "rejects code where the module header is modified" $
      getComment withAdding modUndefined modHeader
      `shouldBe` Right [Additional ["module Main where"
                                   ,"^^^^^^^^^^^^^^^^^"]]
    it "rejects code where imports are added" $
      getComment withAdding modUndefined modImport
      `shouldBe` Right [Additional ["import Data.List"
                                   ,"^^^^^^^^^^^^^^^^"]]
    it "rejects code where pragmas are added" $
      getComment withAdding modUndefined modPragma
      `shouldBe` Right [Additional ["{-# LANGUAGE TemplateHaskell #-}"
                                   ,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"]]
    it "rejects code where patterns are added" $
      getComment noEdits modSum modSumPat
      `shouldBe` Right [Mismatch ["sum xs = undefined"
                                 ,"    ^^^           "
                                 ,"sum [] = 0"
                                 ,"    ^^^   "]
                       ,Additional ["sum (x:xs) = (+) x (sum xs)"
                                   ,"^^^^^^^^^^^^^^^^^^^^^^^^^^^"]]
    describe "with instances" $ do
      it "accepts code where instances are added at the end" $
        getComment withAdding modData modDataInstEnd
        `shouldBe` Right []
      it "accepts code where instances are added in between" $
        getComment withAdding modData modDataInst
        `shouldBe` Right []
      it "accepts code where instances are shifted" $
        getComment noEdits modDataInst modDataInstEnd
        `shouldBe` Right []

data MatchKind a =
    Additional a
  | Match a
  | Missing a
  | Mismatch a
  deriving (Eq, Show)

pretty :: PP.Doc -> String
pretty = unpack . PP.displayT . PP.renderPretty 1.0 100

errorP :: PP.Doc -> Either String b
errorP = Left . pretty

storeP :: PP.Doc -> State [String] ()
storeP = modify . (:) . pretty

retrieve :: State [a] b -> [a]
retrieve = reverse . flip execState []

getComment
  :: SolutionConfig
  -> String
  -> String
  -> Either String [MatchKind [String]]
getComment config template submission = do
  mtemplate  <- parse errorP [] template
  msubmission <- parse errorP [] submission
  case test mtemplate msubmission of
    Fail loc ->
      let state = sequence
            $ rejectMatch storeP config 0 template submission <$> loc
      in Right $ kindOfMatch <$> retrieve state
    Ok _     -> Right []
    Continue -> Left "This should never happen"
  where
    kindOfMatch :: String -> MatchKind [String]
    kindOfMatch x = let x' = lines x in kind x x' (code x')
    code = map (drop 2) . filter ("| " `isPrefixOf`)
    kind l c
      | "Only"         `isInfixOf` l = Additional
      | "Missing"      `isInfixOf` l = Missing
      | "does not fit" `isInfixOf` l = Mismatch
      | otherwise                    = const $ Match c

lines' :: String -> [String]
lines' [] = []
lines' xs = y : zs
  where
    (y, ys) = splitLine xs
    zs      = lines' ys

splitLine :: String -> (String, String)
splitLine []            = ([], [])
splitLine ('\\':'n':xs) = ([], xs)
splitLine (x:xs)        = (x:y, ys)
  where (y, ys) = splitLine xs

modUndefined :: String
modUndefined = [RS.r|
foo :: Int
foo = undefined

main :: IO ()
main = print foo|]

modNoType :: String
modNoType = [RS.r|
foo = undefined

main :: IO ()
main = print foo|]

modUndefinedFunction :: String
modUndefinedFunction = [RS.r|
foo :: Bool -> Int
foo = undefined

main :: IO ()
main = print foo|]

modUndefinedParameter :: String
modUndefinedParameter = [RS.r|
foo :: Bool -> Int
foo t = undefined

main :: IO ()
main = print foo|]

mod42 :: String
mod42 = [RS.r|
foo :: Int
foo = 42

main :: IO ()
main = print foo|]

mod43 :: String
mod43 = [RS.r|
main :: IO ()
main = print foo

foo :: Int
foo = 43|]

modGuards :: String
modGuards = [RS.r|
foo :: Int
foo | False = 23
foo | True = 42

main :: IO ()
main = print foo|]

modGuardsParameter :: String
modGuardsParameter = [RS.r|
foo :: Bool -> Int
foo t | t == False = 23
      | otherwise  = 42

main :: IO ()
main = print foo|]

modRemove :: String
modRemove = [RS.r|
foo :: Int
foo = 42

main = print foo|]

modAdd :: String
modAdd = [RS.r|
foo :: Int
foo = helper

helper :: Int
helper = 42

main :: IO ()
main = print foo
|]

modHeader :: String
modHeader = [RS.r|
module Main where

foo :: Int
foo = undefined

main :: IO ()
main = print foo|]

modImport :: String
modImport = [RS.r|
import Data.List
foo :: Int
foo = undefined

main :: IO ()
main = print foo|]

modPragma :: String
modPragma = [RS.r|
{-# LANGUAGE TemplateHaskell #-}
foo :: Int
foo = undefined

main :: IO ()
main = print foo|]

modSum :: String
modSum = [RS.r|
sum :: [Integer] -> Integer
sum xs = undefined|]

modSumPat :: String
modSumPat = [RS.r|
sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = (+) x (sum xs)|]

modData :: String
modData = [RS.r|
data Foo = Foo

type Bar = Foo

a :: Bool
a = Foo == Foo|]

modDataInst :: String
modDataInst = [RS.r|
data Foo = Foo

instance Eq Foo where
  Foo == Foo = True

type Bar = Foo

a :: Bool
a = Foo == Foo|]

modDataInstEnd :: String
modDataInstEnd = [RS.r|
data Foo = Foo

type Bar = Foo

a :: Bool
a = Foo == Foo

instance Eq Foo where
  Foo == Foo = True|]
