{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Inline where

import qualified Data.Set as S
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Supply

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Pattern
import Sodium.Nucleus.Vector.Name
import Sodium.Nucleus.Vector.Attempt

inline :: (Applicative m, MonadSupply Integer m) => Program -> m Program
inline = recmapped inlineExpression

inlineExpression :: (Applicative m, MonadSupply Integer m) => Expression -> m Expression
inlineExpression (Lambda p a `Beta` x) | not excessive, not dangling = return b
    where (b, count) = runWriter (recmapped w a)
          w = replace $ \case
            e | preciseMatch p e -> Just x
            _ -> Nothing
          excessive = case x of
            Access _ -> False
            OpAccess OpUnit -> False
            _ -> getSum count > 1
          dangling = b `mentions` patBound p
inlineExpression (Lambda p a)
    | (p', names) <- runWriter (patRemoveUnits p)
    , not (S.null names)
    = let (b, _) = runWriter (recmapped w a)
          w = replace $ \case
            Access name | name `S.member` names -> Just (OpAccess OpUnit)
            _ -> Nothing
      in return (Lambda p' b)
inlineExpression e@(Lambda p a)
    | Just ty <- patType p
    , not (patIsAccess p)
    = do
        name <- NameGen <$> supply
        let w = replace $ \case
              e | preciseMatch p e -> Just (Access name)
              _ -> Nothing
            (b, _) = runWriter (recmapped w a)
            dangling = b `mentions` patBound p
        return (if dangling then e else Lambda (PAccess name ty) b)
inlineExpression e = return e

replace :: MonadWriter (Sum Integer) m => Attempt -> Expression -> m Expression
replace fits e
    | Just e' <- fits e = tell (Sum 1) >> return e'
    | otherwise = return e
