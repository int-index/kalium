module Sodium.Nucleus.Vector.Attempt where

import Data.Function
import Control.Applicative
import Sodium.Nucleus.Vector.Program

type Attempt = Expression -> Maybe Expression

taintAttempt :: Attempt -> Attempt
taintAttempt c = pureAttempt tainted where
    tainted = \case
        Taint a -> Taint <$> pureAttempt c a
        _ -> Nothing

pureAttempt :: Attempt -> Attempt
pureAttempt c = fix $ \go -> \case
    Follow p x a -> Follow p x <$> go a
    Into p x a -> Into p x <$> go a
    AppOp3 OpIf xElse xThen cond
        -> AppOp3 OpIf <$> go xElse <*> go xThen <*> pure cond
    a -> c a
