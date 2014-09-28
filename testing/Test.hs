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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Control.Exception (SomeException, Exception, evaluate)

import qualified Sodium

default (T.Text)

main :: IO ()
main = do
    tests <- shelly (silently testGen)
    defaultMain (reifyTests tests)

reifyTests :: [(String, TestGen)] -> TestTree
reifyTests tests = testGroup "tests"
        [ testGroup "test structure" s0
        , testGroup "sodium" s1
        , testGroup "ghc"    s2
        , testGroup "scenarios" s3
        ]
    where (s0, s1, s2, s3) = testRegroup tests

testRegroup = foldr (uncurry go) ([], [], [], [])
    where success = return () :: Assertion
          go name = \case
            Success -> over _4 (testCase name success:)
            TG_Structure a -> over _1 (testCase name a:)
            TG_Sodium a -> over _2 (testCase name a:)
            TG_GHC    a -> over _3 (testCase name a:)
            TG_Scenarios binary scenarios
                        -> over _4 (goldenTestCase name binary scenarios:)

data TestGen = Success
             | TG_Structure Assertion
             | TG_Sodium    Assertion
             | TG_GHC       Assertion
             | TG_Scenarios BS.ByteString [T.Text]

goldenTestCase :: String -> BS.ByteString -> [T.Text] -> TestTree
goldenTestCase name binary scenarios = testGroup name $ do
    scenario <- scenarios
    let scenarioDir = "testing" </> "scenarios" </> P.fromText scenario :: P.FilePath
    return $ goldenVsString (T.unpack scenario)
                            (P.encodeString $ scenarioDir </> "output")
           $ shelly . silently $ do
               inputString <- readfile (scenarioDir </> "input")
               withTmpDir $ \sandbox -> do
                 let ghcBinFile = sandbox </> "ghc_bin"
                 liftIO $ BS.writeFile (P.encodeString ghcBinFile) binary
                 run "chmod" ["u+x", toTextArg ghcBinFile]
                 setStdin inputString
                 LBS.fromStrict . T.encodeUtf8 <$> run ghcBinFile []

testGen :: Sh [(String, TestGen)]
testGen = do
    cd "testing"
    testdirs <- ls "tests" >>= filterM test_d
    forM testdirs $ flip chdir $ do
        testName <- T.unpack . toTextIgnore . P.basename <$> pwd
        tg <- runExceptT $ testStage0 >>= testStage1 >>= testStage2 >>= testStage3
        return (testName, either id id tg)


catch_sh' :: Exception e => (e -> Sh x) -> Sh a -> ExceptT x Sh a
catch_sh' handler action = join $ lift $ catch_sh (fmap return action) (fmap throwError . handler)

testStage0 :: ExceptT TestGen Sh T.Text
testStage0 = catch_sh' handler (readfile "program.pas")
    where handler  :: SomeException -> Sh TestGen
          handler _ = return $ TG_Structure $ assertFailure msg
          msg = "program.pas not found"

testStage1 :: T.Text -> ExceptT TestGen Sh T.Text
testStage1 source = catch_sh' handler action
    where action = liftIO . evaluate
                 $ T.pack . Sodium.translate . T.unpack
                 $ source
          handler :: Sodium.SodiumException -> Sh TestGen
          handler (Sodium.SodiumException s) = return $ TG_Sodium (assertFailure s)

testStage2 :: T.Text -> ExceptT TestGen Sh BS.ByteString
testStage2 source = catch_sh' handler action
   where handler  :: SomeException -> Sh TestGen
         handler _ = return $ TG_GHC (assertFailure (T.unpack source))
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

testStage3 :: BS.ByteString -> ExceptT TestGen Sh TestGen
testStage3 binary = catch_sh' handler action
    where handler  :: SomeException -> Sh TestGen
          handler _ = return $ TG_Structure $ assertFailure msg
          msg = "scenarios not found"
          action = do
            let scenariosFile = "scenarios"
            scenarios <- T.words <$> readfile scenariosFile
            return (TG_Scenarios binary scenarios)
