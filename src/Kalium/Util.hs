{-# LANGUAGE FlexibleContexts #-}
module Kalium.Util where

import Kalium.Prelude
import qualified Data.Map as M

import Control.Monad.Rename
import Control.Monad.Except

type Pairs a b = [(a, b)]
type Endo' a = a -> a
type Kleisli' m a b = a -> m b
type EndoKleisli' m a = Kleisli' m a a

mAsList :: Ord k => Iso' (Map k a) (Pairs k a)
mAsList = iso M.toList M.fromList

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)

closureM :: (Eq a, Monad m) => LensLike' m a a
closureM f = go
  where
    go x = f x >>= \y -> bool (go y) (return x) (x == y)

amaybe :: Alternative f => Maybe a -> f a
amaybe = maybe empty pure

throwEither :: MonadError e m => Either e a -> m a
throwEither = either throwError return

throwMaybe :: MonadError e m => e -> Maybe a -> m a
throwMaybe e = throwEither . maybe (Left e) Right

uniform :: Eq a => [a] -> Maybe a
uniform (x:xs) | all (==x) xs = Just x
uniform _ = Nothing

inContext :: Monad m => (m a -> a -> m b) -> m a -> m b
inContext f ma = ma >>= f ma

zipFilter :: [Bool] -> [a] -> Maybe [a]
zipFilter (keep:keeps) (a:as)
    | keep = (a:) `fmap` zipFilter keeps as
    | otherwise = zipFilter keeps as
zipFilter [] [] = Just []
zipFilter _ _ = Nothing

keepByFst :: (b -> Bool) -> Pairs b a -> [a]
keepByFst p = map snd . filter (p . fst)

far :: EndoCon a -> (h -> a -> Maybe a) -> [h] -> Endo' a
far k f = foldl (toEndoWith . k) id . fmap f

-- Elements are traversed right-to-left
asfar, nofar, sofar :: (h -> a -> Maybe a) -> [h] -> Endo' a

-- Sequentially fuse elements until one fails
asfar = far endoSuccess

-- Fuse with the first successful element
nofar = far endoFailure

-- Sequentially fuse all successful elements
sofar = far endoBoth

data EndoConfig a = EndoConfig (Endo' a) (Endo' a) (Endo' a)
type EndoCon a = Endo' a -> EndoConfig a

endoSuccess, endoFailure, endoBoth :: EndoCon a
endoSuccess fn = EndoConfig id fn id
endoFailure fn = EndoConfig fn id id
endoBoth    fn = EndoConfig id id fn

toEndoWith :: EndoConfig a -> EndoKleisli' Maybe a -> Endo' a
toEndoWith (EndoConfig failure success both) f a
    = both (maybe (failure a) success (f a))

unionWithSame :: (Ord k, Eq v) => Map k v -> Map k v -> Maybe (Map k v)
unionWithSame m1 m2 = sequenceA $ M.unionWith same (pure <$> m1) (pure <$> m2)
  where
    same (Just x) (Just y) | x == y = Just x
    same _ _ = Nothing

type MonadNameGen m = MonadRename Integer String m
