module Sodium.Util where

import Control.Lens
import Data.Bool
import qualified Data.Map as M

type Pairs a b = [(a, b)]

mAsList :: Ord k => Iso' (M.Map k a) (Pairs k a)
mAsList = iso M.toList M.fromList

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)

closureM :: (Eq a, Monad m) => (a -> m a) -> (a -> m a)
closureM f = go where
    go x = f x >>= \y -> bool (go y) (return x) (x == y)

uniform :: Eq a => [a] -> Maybe a
uniform (x:xs) | all (==x) xs = Just x
uniform _ = Nothing

zipFilter :: [Bool] -> [a] -> Maybe [a]
zipFilter (keep:keeps) (a:as)
    | keep = (a:) `fmap` zipFilter keeps as
    | otherwise = zipFilter keeps as
zipFilter [] [] = Just []
zipFilter _ _ = Nothing
