module Sodium.Tr
	( head
	, before
	, fallback
	, trap
	, trapGuard
	, expect
	) where

import Prelude hiding (head, until)
import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy

head :: MonadPlus m => StateT [x] m x
head = StateT $ \case
	(x:xs) -> return (x, xs)
	[] -> mzero

before
	:: (Functor m, MonadPlus m)
	=> StateT x m a
	-> StateT x m b
	-> StateT x m (b, [a])
before u v
	 =  flip (,) [] <$> v
	<|> (\a -> fmap (a:)) <$> u <*> (u `before` v)

fallback :: Alternative f => a -> f a -> f a
fallback = flip (<|>) . pure

trap
	:: Monad m
	=> StateT x m (a -> b)
	-> (x -> m a)
	-> (x -> m b)
trap tr next
	= \x -> do
		(a, y) <- runStateT tr x
		b <- next y
		return $ a b

trapGuard
	:: (Functor m, MonadPlus m)
	=> StateT x m b
	-> (x -> Bool)
	-> (x -> m b)
trapGuard tr p
	= trap (const <$> tr) (guard . p)

expect :: (Eq x, MonadPlus m) => x -> StateT [x] m x
expect x = mfilter (==x) head
