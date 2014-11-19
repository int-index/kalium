module Main (main) where

import Control.Monad.Except

import System.Environment
import System.Exit
import qualified Sodium
import Sodium.Error ()

main = getArgs >>= \case
    [filename1, filename2] -> processFiles filename1 filename2
    _ -> do putStrLn "usage: sodium filename.pas filename.hs"
            exitWith $ ExitFailure 1

processFiles :: String -> String -> IO ()
processFiles filename1 filename2 = do
    content <- readFile filename1
    case runExcept (Sodium.translate content) of
        Left e  -> putStrLn (show e)
        Right (log, r) -> do
            forM log $ \repr -> do
                putStrLn repr
                putStr "\n\n"
            writeFile filename2 r
