module Sodium.Nucleus.Vector.Attempt where

import Data.Function
import Control.Applicative
import Control.Lens
import Sodium.Nucleus.Vector.Program

type Attempt = Expression -> Maybe Expression

taintAttempt :: Attempt -> Attempt
taintAttempt c = propagate tainted where
    tainted = \case
        Taint a -> Taint <$> propagate c a
        _ -> Nothing

propagate :: Applicative m => LensLike' m Expression Expression
propagate c = fix $ \go -> \case
    Follow p x a -> Follow p x <$> go a
    Into p x a -> Into p x <$> go a
    AppOp2 OpBindIgnore x a -> AppOp2 OpBindIgnore x <$> go a
    AppOp3 OpIf xElse xThen cond
        -> AppOp3 OpIf <$> go xElse <*> go xThen <*> pure cond
    a -> c a
