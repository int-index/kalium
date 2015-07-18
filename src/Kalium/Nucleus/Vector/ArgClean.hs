{-# LANGUAGE FlexibleContexts #-}
module Kalium.Nucleus.Vector.ArgClean where

import Kalium.Prelude
import Kalium.Util

import Control.Monad.Trans.Maybe

import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Recmap
import Kalium.Nucleus.Vector.FuncUpdate

argClean :: (MonadNameGen m) => EndoKleisli' m Program
argClean = funcUpdate funcArgClean

funcArgClean :: (MonadNameGen m) => FuncUpdate m
funcArgClean name name' (Func ty a) = do
    let (ps, b) = unlambda a
        (ns, ps') = patsArgClean ps
        a' = lambda ps' b
    ty' <- (MaybeT . pure) (tyArgClean ns ty)
    guard (ty' /= ty)
    let upd = recmapped %~ tryApply (appArgClean name ns name')
    return (upd, Func ty' a')

patsArgClean :: [Pattern] -> ([Bool], [Pattern])
patsArgClean [] = ([], [])
patsArgClean (p:ps) =
    let (ns, ps') = patsArgClean ps
    in case p of
        PWildCard -> (False:ns, ps')
        _ -> (True:ns, p:ps')

tyArgClean :: [Bool] -> Type -> Maybe Type
tyArgClean [] ty = Just ty
tyArgClean (n:ns) (TypeApp2 TypeFunction ty1 ty2) = wrap <$> tyArgClean ns ty2
    where wrap | n = TypeApp2 TypeFunction ty1
               | otherwise = id
tyArgClean _ _ = Nothing

appArgClean :: Name -> [Bool] -> Name -> Expression -> Maybe Expression
appArgClean name ns name' e
    | (Access op:es) <- unbeta e, op == name
     = beta <$> ((Access name':) <$> zipFilter ns es)
appArgClean _ _ _ _ = Nothing
