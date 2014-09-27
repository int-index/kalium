{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Monad.Except

import Shelly
import qualified Filesystem.Path.CurrentOS as P
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception (SomeException, Exception, evaluate)

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
            Stage0_Gen a -> over _1 (testCase name a:)
            Stage1_Gen a -> over _2 (testCase name a:)
            Stage2_Gen a -> over _3 (testCase name a:)
            Stage3_Gen _ -> over _4 (testCase name a:)
                                              where a=return()

data TestGen = Stage0_Gen Assertion
             | Stage1_Gen Assertion
             | Stage2_Gen Assertion
             | Stage3_Gen BS.ByteString

testGen :: Sh [(String, TestGen)]
testGen = do
    testdirs <- ls "tests" >>= filterM test_d
    forM testdirs $ flip chdir $ do
        testName <- T.unpack . toTextIgnore . P.basename <$> pwd
        tg <- runExceptT $ testStage0 >>= testStage1 >>= testStage2
        return (testName, either id Stage3_Gen tg)


catch_sh' :: Exception e => (e -> Sh x) -> Sh a -> ExceptT x Sh a
catch_sh' handler action = join $ lift $ catch_sh (fmap return action) (fmap throwError . handler)

testStage0 :: ExceptT TestGen Sh T.Text
testStage0 = catch_sh' handler (readfile "program.pas")
    where handler  :: SomeException -> Sh TestGen
          handler _ = return $ Stage0_Gen $ assertFailure msg
          msg = "bad test structure: program.pas not found"

testStage1 :: T.Text -> ExceptT TestGen Sh T.Text
testStage1 source = catch_sh' handler action
    where action = liftIO . evaluate
                 $ T.pack . Sodium.translate . T.unpack
                 $ source
          handler :: Sodium.SodiumException -> Sh TestGen
          handler (Sodium.SodiumException s) = return $ Stage1_Gen (assertFailure s)

testStage2 :: T.Text -> ExceptT TestGen Sh BS.ByteString
testStage2 source = catch_sh' handler action
   where handler  :: SomeException -> Sh TestGen
         handler _ = return $ Stage2_Gen (assertFailure (T.unpack source))
         action = withTmpDir $ \sandbox -> do
           let mainFile   = sandbox </> "Main.hs"
           let ghcObjDir  = sandbox </> "ghc_obj"
           let ghcBinFile = sandbox </> "ghc_bin"
           writefile mainFile source
           mkdir ghcObjDir
           run "ghc"
               [ toTextArg mainFile
               , T.pack "-outputdir", toTextArg ghcObjDir
               , T.pack "-o", toTextArg ghcBinFile
               ] & errExit False
           readBinary ghcBinFile
