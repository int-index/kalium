module Sodium.ApplyOnce where

import Control.Applicative
import Control.Monad

data ApplyOnce a
	= Once a
	| None a
	| Ambiguous

instance Functor ApplyOnce where
	fmap = liftM

instance Applicative ApplyOnce where
	pure = return
	(<*>) = ap

instance Monad ApplyOnce where
	return = None
	Ambiguous >>= _ = Ambiguous
	None a >>= f = f a
	Once a >>= f = case f a of
		None b -> Once b
		_ -> Ambiguous
