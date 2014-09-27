{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Lens

import Shelly
import qualified Filesystem.Path.CurrentOS as P
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception (evaluate)

import qualified Sodium

default (T.Text)

main :: IO ()
main = do
    tests <- shelly (silently testGen)
    defaultMain (reifyTests tests)

reifyTests :: [(String, TestGen)] -> TestTree
reifyTests tests = testGroup "tests"
        [ testGroup "stage 0" s0
        , testGroup "stage 1" s1
        , testGroup "stage 2" s2
        , testGroup "stage 3" s3
        ]
    where (s0, s1, s2, s3) = testRegroup tests

testRegroup = foldr (uncurry go) ([], [], [], [])
    where go name = \case
            Stage0_Fail a -> over _1 (testCase name a:)
            Stage1_Fail a -> over _2 (testCase name a:)
            Stage2_Fail a -> over _3 (testCase name a:)
            Stage3_Success-> over _4 (testCase name a:)
                                              where a=return()

data TestGen = Success
             | Stage0_Fail Assertion
             | Stage1_Fail Assertion
             | Stage2_Fail Assertion
             | Stage3_Success

testGen :: Sh [(String, TestGen)]
testGen = do
    testdirs <- ls "tests"
    forM testdirs $ \testdir -> do
        chdir testdir $ do
            testName <- T.unpack . toTextIgnore . P.basename <$> pwd
            tg <- testStage0 >>= \case
                Left t -> return t
                Right stage0 -> do
                    testStage1 stage0 >>= \case
                        Left t -> return t
                        Right stage1 -> do
                            testStage2 stage1 >>= \case
                                Left t -> return t
                                Right stage3 -> return Stage3_Success
            return (testName, tg)

testStage0 :: Sh (Either TestGen T.Text)
testStage0 = catchany_sh (Right <$> readfile "program.pas")
           $ \_ -> return $ Left $ Stage0_Fail (assertFailure msg)
    where msg = "bad test structure: program.pas not found"

testStage1 :: T.Text -> Sh (Either TestGen T.Text)
testStage1 source = catch_sh (Right <$> result)
                  $ \(Sodium.SodiumException s) -> return $ Left $ Stage1_Fail (assertFailure s)
    where result = liftIO . evaluate
                 $ T.pack . Sodium.translate . T.unpack
                 $ source

testStage2 :: T.Text -> Sh (Either TestGen BS.ByteString)
testStage2 source = do
    withTmpDir $ \sandbox -> do
        let mainFile   = sandbox </> "Main.hs"
        let ghcObjDir  = sandbox </> "ghc_obj"
        let ghcBinFile = sandbox </> "ghc_bin"
        writefile mainFile source
        mkdir ghcObjDir
        errExit False
          $ run "ghc" [ toTextArg mainFile
                      , T.pack "-outputdir", toTextArg ghcObjDir
                      , T.pack "-o", toTextArg ghcBinFile
                      ]
        catchany_sh (Right <$> readBinary ghcBinFile)
                   $ \_ -> return $ Left $ Stage2_Fail (assertFailure (T.unpack source))
