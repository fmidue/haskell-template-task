{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fforce-recomp #-}
module Haskell.Template.FileContents (
  testHarnessContents,
  testHelperContents,
  ) where

import Language.Preprocessor.Cpphs (
  BoolOptions(..),
  CpphsOptions(..),
  defaultBoolOptions,
  defaultCpphsOptions,
  runCpphs
  )
import Language.Haskell.TH              (runIO, stringE)
import System.FilePath                  ((</>))
import TH.RelativePaths                 (pathRelativeToCabalPackage)

testHelperContents :: String
testHelperContents  =
  $(do file     <- pathRelativeToCabalPackage
         $ "embedded" </> "src" </> "TestHelper.hs"
       contents <- runIO $ readFile file >>= runCpphs defaultCpphsOptions {
         defines = [
#ifdef IOTASKS
             ("IOTASKS", "")
#else

#endif
           ],
         boolopts = defaultBoolOptions {locations = False}
         }
         ""
       stringE contents)

testHarnessContents :: String
testHarnessContents =
  $(do file     <- pathRelativeToCabalPackage
         $ "embedded" </> "src" </> "TestHarness.hs"
       contents <- runIO $ readFile file
       stringE contents)
