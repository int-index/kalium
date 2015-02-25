module Kalium.Nucleus.Vector.Attempt where

import Kalium.Prelude
import Kalium.Util

import Kalium.Nucleus.Vector.Program

tainting :: Alternative m => Endo' (LensLike' m Expression Expression)
tainting prop c = prop $ \case
    Taint a -> Taint <$> prop c a
    _ -> empty

propagate :: Applicative m => LensLike' m Expression Expression
propagate c = fix $ \go -> \case
    Follow p x a -> Follow p x <$> go a
    Into p x a -> Into p x <$> go a
    AppOp2 OpBindIgnore x a -> AppOp2 OpBindIgnore x <$> go a
    AppOp3 OpIf xElse xThen cond
        -> AppOp3 OpIf <$> go xElse <*> go xThen <*> pure cond
    a -> c a
