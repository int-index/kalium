{-# LANGUAGE FlexibleContexts #-}

module Data.Stack
	( Stack
	, evalStack
	, push
	, pop
	) where

import Control.Applicative
import Control.Monad.State.Strict

type Stack a = State [a]

pop :: (Applicative m, MonadState [a] m) => m a
pop = gets head <* modify tail

push :: (Applicative m, MonadState [a] m) => a -> m ()
push a = modify (a:)

evalStack = evalState
