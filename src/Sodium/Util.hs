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
