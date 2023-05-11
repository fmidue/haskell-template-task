{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fforce-recomp #-}
module Haskell.Template.FileContents (
  testHarnessContents,
  testHelperContents,
  ) where

import Language.Haskell.TH              (runIO, stringE)
import System.FilePath                  ((</>))
import TH.RelativePaths                 (pathRelativeToCabalPackage)

testHelperContents :: String
testHelperContents  =
  $(do file     <- pathRelativeToCabalPackage
         $ "src" </> "TestHelper.hs"
       contents <- runIO $ readFile file
       stringE contents)

testHarnessContents :: String
testHarnessContents =
  $(do file     <- pathRelativeToCabalPackage
         $ "src" </> "TestHarness.hs"
       contents <- runIO $ readFile file
       stringE contents)
