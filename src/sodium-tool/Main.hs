module Main (main) where

import System.Environment
import System.Exit
import qualified Sodium

main = getArgs >>= \case
	[filename1, filename2] -> processFiles filename1 filename2
	_ -> do
		putStrLn "usage: sodium filename.pas filename.hs"
		exitWith $ ExitFailure 1

processFiles :: String -> String -> IO ()
processFiles filename1 filename2 = do
	source <- readFile filename1
	let dest = Sodium.translate source
	putStrLn dest
	writeFile filename2 dest
