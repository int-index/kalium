{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Context where

import Sodium.Prelude
import Sodium.Util

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name
import Sodium.Nucleus.Vector.Attempt

contexts
    :: MonadWriter [Expression] m
    => EndoKleisli' Maybe Expression
    -> EndoKleisli' m     Expression
contexts fits (Beta cxt (fits -> Just e)) = tell [cxt] >> return e
contexts _ e = return e

context :: (Applicative m, MonadNameGen m) => Name -> Expression
        -> (Name -> Expression -> Maybe Expression -> m a) -> m a
context name a cont = do
    name' <- NameGen <$> mkname Nothing
    let (b, cxts) = runWriter (recmapped w a)
        dangling = b `mentions` name
        w = contexts $ \case
            Access ((==) name -> True) -> Just (Access name')
            _ -> Nothing
    cont name' b $ guard (not dangling) >> uniform cxts

extractCtx :: (Applicative m, MonadNameGen m) => EndoKleisli' m Program
extractCtx = recmapped extractCtxExpression

extractCtxExpression
    :: (Applicative m, MonadNameGen m)
    => EndoKleisli' m Expression
extractCtxExpression = \case
    e@(Follow (PAccess name ty) x a) -> do
        context name a $ \name' b -> \case
            Just (OpAccess op)
                | Just x'  <- tainting propagate (return . AppOp1 op) x
                , Just ty' <- hackish_typeApp op ty
                -> return $ Follow (PAccess name' ty') x' b
            _ -> return e
    e -> return e

-- TODO: typecheck
hackish_typeApp :: NameSpecial -> Type -> Maybe Type
hackish_typeApp = \case
    OpFst -> \case
        TypeApp2 TypePair ty _ -> pure ty
        _ -> empty
    OpSnd -> \case
        TypeApp2 TypePair _ ty -> pure ty
        _ -> empty
    _ -> \_ -> empty
