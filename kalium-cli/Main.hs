{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Traversable
import Control.Monad.Except

import Options.Applicative

import qualified Kalium

data Options = Options
    { filenameIn  :: String
    , filenameOut :: String
    , logSwitch :: Bool
    , patSigSwitch :: Bool
    }

pOptions :: Parser Options
pOptions = Options
    <$> strArgument (metavar "FILENAME.PAS" <> help "Source Pascal program")
    <*> strArgument (metavar "FILENAME.HS"  <> help "Target Haskell program")
    <*> switch (long "log" <> help "Print intermediate results")
    <*> switch (long "pat" <> help "Generate pattern signatures")

handleOptions :: ParserInfo Options
handleOptions = info (helper <*> pOptions) (header "Kalium")

main :: IO ()
main = execParser handleOptions >>= processFiles

processFiles :: Options -> IO ()
processFiles Options{..} = do
    content <- readFile filenameIn
    case runExcept (Kalium.translate patSigSwitch content) of
        Left e -> print e
        Right (log, r) -> do
            when logSwitch $ do
                void $ for log $ \repr -> do
                    putStrLn repr
                    putStr "\n\n"
            writeFile filenameOut r
