{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Inline where

import Control.Lens
import Control.Monad.Writer

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Pattern
import Sodium.Nucleus.Vector.Name

inline :: Program -> Program
inline = over recmapped inlineExpression

inlineExpression :: Expression -> Expression
inlineExpression (Lambda p a `Beta` x) | not excessive, not dangling = b
    where (b, count) = runWriter (recmapped (inl x p) a)
          excessive = case x of
            Access _ -> False
            OpAccess OpUnit -> False
            _ -> getSum count > 1
          dangling = b `mentions` patBound p
inlineExpression e = e

inl :: MonadWriter (Sum Integer) m => Expression -> Pattern -> Expression -> m Expression
inl replacement model e | preciseMatch model e = do
    tell (Sum 1)
    return replacement
inl _ _ e = return e
