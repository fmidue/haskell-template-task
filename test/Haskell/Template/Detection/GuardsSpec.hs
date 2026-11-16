{-# LANGUAGE QuasiQuotes #-}

module Haskell.Template.Detection.GuardsSpec where

import Data.Bifunctor (bimap)
import qualified Data.String.Interpolate as SI (i)
import Haskell.Template.Detection.Guards (detectRedundantGuardCondition)
import Language.Haskell.Exts (ParseResult (..), parse, prettyPrint)
import Language.Haskell.Names (annotate, loadBase)
import Test.Hspec (Spec, describe, it, shouldReturn)

detect :: String -> IO [(String, [String])]
detect code = case parse code of
  ParseOk mod' -> do
    baseEnv <- loadBase
    redundant <- detectRedundantGuardCondition $ annotate baseEnv mod'
    pure $ map (bimap prettyPrint (map prettyPrint)) redundant
  _ -> fail "failed to parse program"

spec :: Spec
spec =
  describe "redundant guard condition detection" $ do
    it "should find one redundant guard condition" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 0 = 0
  | t >= 0 = 1
|]
      detect code `shouldReturn` [("test", ["t >= 0"])]
    it "should find multiple redundant guard conditions" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 0 = 0
  | t >= 0 && t < 3 = 1
  | t >= 0 && t >= 3 && t < 9 = 2
|]
      detect code `shouldReturn` [("test", ["t >= 0", "t >= 0", "t >= 3"])]
    it "should not find redundant guard conditions" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 0 = 0
  | t >= 1 && t < 3 = 1
  | t < 9 = 2
|]
      detect code `shouldReturn` []
    it "should not find redundant guard conditions" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 3.9 = 0
  | t >= 4 && t < 6 = 1
  | t < 9 = 2
|]
      detect code `shouldReturn` []
    it "should not find redundant guard conditions (wrong scope)" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 0 = 0
  | (let t = 99 in t >= 0) && t < 6 = 1
  | t < 9 = 2
|]
      detect code `shouldReturn` []
    it "should find redundant guard condition (negated or)" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 0 || t > 0 = 0
  | t == 0 = 1
|]
      detect code `shouldReturn` [("test", ["t == 0"])]
    it "should not find redundant guard conditions (otherwise)" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 0 || t > 0 = 0
  | otherwise = 1
|]
      detect code `shouldReturn` []
    it "should find redundant guard conditions (nested)" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 4 = 0
  | t >= 4 && t < 9 = 1
  | t >= 4 && (t >= 9 && t < 20) = 1
|]
      detect code `shouldReturn` [("test", ["t >= 4", "t >= 4", "t >= 9"])]
    it "should find redundant guard conditions (and application)" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 4 = 0
  | t >= 4 && t < 9 = 1
  | and [t >= 4, t >= 9, t < 20] = 1
|]
      detect code `shouldReturn` [("test", ["t >= 4", "t >= 4", "t >= 9"])]
    it "should find redundant guard conditions (comma separated)" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 4 = 0
  | t >= 4 && t < 9 = 1
  | t >= 4, t >= 9, t < 20 = 1
|]
      detect code `shouldReturn` [("test", ["t >= 4", "t >= 4", "t >= 9"])]
    it "should find redundant guard condition (sum application)" $ do
      let code =
            [SI.i|module Main where
test t
  | t < sum [1,1,2] = 0
  | t >= 4 && t < 9 = 1
|]
      detect code `shouldReturn` [("test", ["t >= 4"])]
    it "should find redundant guard condition (product application)" $ do
      let code =
            [SI.i|module Main where
test t
  | t < 4 = 0
  | t >= product [2, 2] && t < 9 = 1
|]
      detect code `shouldReturn` [("test", ["t >= product [2, 2]"])]
    it "should find redundant guard condition (infix div application)" $ do
      let code =
            [SI.i|module Main where
test t
  | t `div` 2 == 4 = 0
  | t /= 8 = 1
|]
      detect code `shouldReturn` [("test", ["t /= 8"])]
    it "should find redundant guard condition (prefix div application" $ do
      let code =
            [SI.i|module Main where
test t
  | div t 2 == 4 = 0
  | t /= 8 = 1
|]
      detect code `shouldReturn` [("test", ["t /= 8"])]
    it "should find redundant guard condition ( / application)" $ do
      let code =
            [SI.i|module Main where
test t
  | t / 2 == 4 = 0
  | t /= 8 = 1
|]
      detect code `shouldReturn` [("test", ["t /= 8"])]
    it "should find redundant guard condition on list" $ do
      let code =
            [SI.i|module Main where
test xs
  | null xs = 0
  | length xs >= 1 = 1
|]
      detect code `shouldReturn` [("test", ["length xs >= 1"])]
    it "should find redundant guard condition (not application)" $ do
      let code =
            [SI.i|module Main where
test xs
  | null xs = 0
  | not (null xs) = 1
|]
      detect code `shouldReturn` [("test", ["not (null xs)"])]
    it "should ignore generators" $ do
      let code =
            [SI.i|module Main where
test t
  | 1 <- t = 0
  | t /= 1 = 1
|]
      detect code `shouldReturn` []
