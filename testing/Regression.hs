{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (FilePath)
import Control.Monad.Except
import Control.Monad.Managed
import Turtle
import Filesystem.Path.CurrentOS (encodeString)
import System.IO.Silently (silence)
import System.Directory
import System.Process
import qualified Data.Text as Text
import qualified Control.Foldl as Fold
import Test.Tasty
import Test.Tasty.HUnit

import qualified Kalium

main :: IO ()
main = getTests >>= defaultMain . testGroup ""

data TestDescription
    = TestScenarios  FilePath [(Text, FilePath)]
    | TestShouldfail FilePath

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
        scenarios <- strict $ input (path </> "scenarios")
        return $ testScenarios name (Text.words scenarios)

testShouldfail :: Text -> TestTree
testShouldfail name = testCase (Text.unpack name) $ do
    source <- (strict . input) (testsDir </> fromText name </> "program.pas")
    case runExcept (Kalium.translate True (Text.unpack source)) of
      Left _ -> return ()
      Right _ -> assertFailure "should fail"

testScenarios :: Text -> [Text] -> TestTree
testScenarios name scenarios = testGroup (Text.unpack name)
    (testScenario name `map` scenarios)

chmod :: MonadIO io => FilePath -> (Permissions -> Permissions) -> io ()
chmod path f = liftIO $ do
    let path' = encodeString path
    getPermissions path' >>= setPermissions path' . f

testScenario :: Text -> Text -> TestTree
testScenario name scenario = testCase (Text.unpack scenario) $ do
    source <- (strict . input) (testsDir </> fromText name </> "program.pas")
    case runExcept (Kalium.translate True (Text.unpack source)) of
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
        chmod ghcBinPath (setOwnerExecutable True)
        let scenarioPath = scenariosDir </> fromText scenario
        (excode, msg, _) <- readProcessWithExitCode
            (encodeString scenarioPath) [] (encodeString ghcBinPath)
        case (excode, msg) of
            (ExitSuccess, "") -> return ()
            (_, msg) -> assertFailure msg
