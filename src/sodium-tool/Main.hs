{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Traversable
import Control.Monad.Except

import Options.Applicative

import qualified Sodium
import Sodium.Error ()

data Options = Options
    { filenameIn  :: String
    , filenameOut :: String
    , logSwitch :: Bool
    }

pOptions :: Parser Options
pOptions = Options
    <$> strArgument (metavar "FILENAME.PAS" <> help "Source Pascal program")
    <*> strArgument (metavar "FILENAME.HS"  <> help "Target Haskell program")
    <*> switch (long "log" <> help "Print intermediate results")

handleOptions :: ParserInfo Options
handleOptions = info (helper <*> pOptions) (header "sodium")

main :: IO ()
main = do
    Options {..} <- execParser handleOptions
    processFiles filenameIn filenameOut logSwitch

processFiles :: String -> String -> Bool -> IO ()
processFiles filenameIn filenameOut logSwitch = do
    content <- readFile filenameIn
    case runExcept (Sodium.translate content) of
        Left e -> print e
        Right (log, r) -> do
            when logSwitch $ do
                void $ for log $ \repr -> do
                    putStrLn repr
                    putStr "\n\n"
            writeFile filenameOut r
