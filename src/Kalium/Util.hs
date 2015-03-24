module Kalium.Util where

import Kalium.Prelude
import qualified Data.Map as M

type Pairs a b = [(a, b)]
type Endo' a = a -> a
type Kleisli' m a b = a -> m b
type EndoKleisli' m a = Kleisli' m a a

mAsList :: Ord k => Iso' (Map k a) (Pairs k a)
mAsList = iso M.toList M.fromList

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)

closureM :: (Eq a, Monad m) => LensLike' m a a
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

keepByFst :: (b -> Bool) -> Pairs b a -> [a]
keepByFst p = map snd . filter (p . fst)

far k f = foldr (\h go -> k go (f h)) id

asfar :: (h -> a -> Maybe a) -> [h] -> (a -> a)
asfar = far $ \go -> toEndoWith id go id

sofar :: (h -> a -> Maybe a) -> [h] -> (a -> a)
sofar = far $ \go -> toEndoWith id id go

nofar :: (h -> a -> Maybe a) -> [h] -> (a -> a)
nofar = far $ \go -> toEndoWith go id id

toEndoWith
    :: (a -> a) -- failure
    -> (a -> a) -- success
    -> (a -> a) -- both
    -> (a -> Maybe a)
    -> (a -> a)
toEndoWith failure success both f = \a -> both (maybe (failure a) success (f a))


unionWithSame :: (Ord k, Eq v) => Map k v -> Map k v -> Maybe (Map k v)
unionWithSame m1 m2 = sequenceA $ M.unionWith same (pure <$> m1) (pure <$> m2)
  where
    same (Just x) (Just y) | x == y = Just x
    same _ _ = Nothing

type MonadNameGen m = MonadRename Integer String m
