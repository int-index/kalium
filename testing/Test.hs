{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Monad.Except

import System.Process
import System.Exit
import System.IO
import System.Directory
import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (myThreadId)
import Control.Exception

import qualified Sodium

main :: IO ()
main = do
    tests <- testGen
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
            TG_Scenarios as -> over _4 (tg:)
                where tg = testGroup name (uncurry testCase `map` as)

data TestGen = Success
             | TG_Structure Assertion
             | TG_Sodium    Assertion
             | TG_GHC       Assertion
             | TG_Scenarios [(TestName, Assertion)]

testGen :: IO [(String, TestGen)]
testGen = chdir "testing" $ do
    chdir "tests" $ do
        dirs <- getCurrentDirectory >>= getDirectoryContents
        let testdirs = filter (not . isPrefixOf ".") dirs
        forM testdirs $ \testdir -> chdir testdir $ do
            tg <- runExceptT $ testStage0 >>= testStage1 >>= testStage2 >>= testStage3
            return (testdir, either id id tg)


catch' :: Exception e => (e -> IO x) -> IO a -> ExceptT x IO a
catch' handler action = join $ lift $ catch (fmap return action) (fmap throwError . handler)

testStage0 :: ExceptT TestGen IO (Bool, String)
testStage0 = catch' handler action
    where handler  :: SomeException -> IO TestGen
          handler _ = return $ TG_Structure $ assertFailure msg
          msg = "program.pas not found"
          action = (,) <$> doesFileExist "shouldfail" <*> readFile "program.pas"

testStage1 :: (Bool, String) -> ExceptT TestGen IO String
testStage1 (shouldfail, source)
        = withExceptT handle (Sodium.translate source)
    where handle _ | shouldfail = Success
          handle e = TG_Sodium (assertFailure (show e))

testStage2 :: String -> ExceptT TestGen IO BS.ByteString
testStage2 source = catch' handler action
   where handler  :: SomeException -> IO TestGen
         handler _ = return $ TG_GHC (assertFailure source)
         action = withTmpDir $ \sandbox -> chdir sandbox $ do
           let mainFile   = "Main.hs"
           let ghcObjDir  = "ghc_obj"
           let ghcBinFile = "ghc_bin"
           writeFile mainFile source
           createDirectory ghcObjDir
           callProcess "ghc"
               [ mainFile
               , "-outputdir", ghcObjDir
               , "-o", ghcBinFile
               ]
           BS.readFile ghcBinFile

testStage3 :: BS.ByteString -> ExceptT TestGen IO TestGen
testStage3 binary = catch' handler action
    where handler  :: SomeException -> IO TestGen
          handler _ = return $ TG_Structure $ assertFailure msg
          msg = "scenarios not found"
          action = do
            let scenariosFile = "scenarios"
            scenarios <- words <$> readFile scenariosFile
            return (tg_scenarios binary scenarios)

tg_scenarios :: BS.ByteString -> [String] -> TestGen
tg_scenarios binary scenarios = TG_Scenarios (map tg scenarios)
    where tg :: String -> (TestName, Assertion)
          tg scenario = (scenario, tg_scenario binary scenario)

tg_scenario :: BS.ByteString -> String -> Assertion
tg_scenario binary scenario = do
    let scenarioPath = "testing/scenarios" ++ "/" ++ scenario
    withTmpDir $ \sandbox -> do
        let ghcBinFile = sandbox ++ "/" ++ "ghc_bin"
        do BS.writeFile ghcBinFile binary
           p <- getPermissions ghcBinFile
           setPermissions ghcBinFile (setOwnerExecutable True p)
        (excode, msg, _) <- readProcessWithExitCode scenarioPath [] ghcBinFile
        case (excode, msg) of
            (ExitSuccess, "") -> return ()
            (_, msg) -> assertFailure msg

chdir :: FilePath -> (IO a -> IO a)
chdir path act = do
    dir <- getCurrentDirectory
    setCurrentDirectory path
    act <* setCurrentDirectory dir

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir act = do
  dir <- getTemporaryDirectory
  tid <- myThreadId
  (p, handle) <- openTempFile dir ("tmp" ++ filter isAlphaNum (show tid))
  hClose handle -- required on windows
  callProcess "rm" ["-f", p]
  createDirectory p
  act p `finally` callProcess "rm" ["-rf", p]
