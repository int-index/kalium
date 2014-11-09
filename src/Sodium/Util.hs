module Sodium.Util where

import Control.Lens
import qualified Data.Map as M

type Pairs a b = [(a, b)]

mAsList :: Ord k => Iso' (M.Map k a) (Pairs k a)
mAsList = iso M.toList M.fromList

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)
