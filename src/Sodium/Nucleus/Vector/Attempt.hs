module Sodium.Nucleus.Vector.Attempt where

import Data.Function
import Control.Applicative
import Sodium.Nucleus.Vector.Program

type Attempt = Expression -> Maybe Expression

taintAttempt :: Attempt -> Attempt
taintAttempt c = fix $ \go -> \case
    Taint a -> Taint <$> c a
    Follow p x a -> Follow p x <$> go a
    AppOp3 OpIf xElse xThen cond
        -> AppOp3 OpIf <$> go xElse <*> go xThen <*> pure cond
    _ -> Nothing
