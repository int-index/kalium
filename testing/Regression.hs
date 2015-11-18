{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (FilePath)
import Control.Monad.Except
import Turtle
import Filesystem.Path.CurrentOS (encodeString)
import System.IO.Silently (silence)
import System.Process
import qualified Data.Text as Text
import qualified Control.Foldl as Fold
import Test.Tasty
import Test.Tasty.HUnit

import qualified Kalium

main :: IO ()
main = getTests >>= defaultMain . testGroup ""

testingDir, testsDir :: FilePath
testingDir = "testing"
testsDir = testingDir </> "tests"
scenariosDir = testingDir </> "scenarios"

getTests :: IO [TestTree]
getTests = flip fold Fold.list $ do
    path <- ls testsDir
    let name = (Text.pack . encodeString) (basename path)
    shouldfail <- testfile (path </> "shouldfail")
    if shouldfail
      then return (testShouldfail name)
      else do
        scenarios <- (strict . input) (path </> "scenarios")
        return $ testScenarios name (Text.words scenarios)

translating name cont = do
  source <- (strict . input) (testsDir </> fromText name </> "program.pas")
  cont $ runExcept (Kalium.translate True (Text.unpack source))

testShouldfail :: Text -> TestTree
testShouldfail name
  = testCase (Text.unpack name)
  $ translating name $ \case
      Left _ -> return ()
      Right _ -> assertFailure "should fail"

testScenarios :: Text -> [Text] -> TestTree
testScenarios name scenarios
  = testGroup (Text.unpack name)
  $ map (testScenario name) scenarios

testScenario :: Text -> Text -> TestTree
testScenario name scenario
  = testCase (Text.unpack scenario)
  $ translating name $ \case
      Left e -> assertFailure (show e)
      Right (_, src) -> with (mktempdir "/tmp" "kalium") $ \tmpdir -> do
        let
            mainPath   = tmpdir </> "Main.hs"
            ghcObjPath = tmpdir </> "ghc_obj"
            ghcBinPath = tmpdir </> "ghc_bin"
        mkdir ghcObjPath
        output mainPath (pure (Text.pack src))
        silence $ callProcess "ghc"
            [ encodeString mainPath
            , "-outputdir", encodeString ghcObjPath
            , "-o", encodeString ghcBinPath ]
        chmod executable ghcBinPath
        let scenarioPath = scenariosDir </> fromText scenario
        (excode, msg, _) <- readProcessWithExitCode
            (encodeString scenarioPath) [] (encodeString ghcBinPath)
        case (excode, msg) of
            (ExitSuccess, "") -> return ()
            (_, msg) -> assertFailure msg
