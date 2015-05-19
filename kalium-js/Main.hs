module Main where

import qualified Kalium
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
    t <- textArea def
    r <- mapDyn trans (_textArea_value t)
    el "pre" (dynHtml r)

trans :: String -> String
trans s = case Kalium.translate False s of
    Left e -> "Could not translate the provided source:\n" ++ show e
    Right (log, r) -> r
