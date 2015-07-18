{-# LANGUAGE FlexibleContexts #-}
module Kalium.Nucleus.Vector.RetClean where

import Kalium.Prelude
import Kalium.Util

import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Recmap
import Kalium.Nucleus.Vector.FuncUpdate

retClean :: (MonadNameGen m) => EndoKleisli' m Program
retClean = funcUpdate funcRetClean

funcRetClean :: (MonadNameGen m) => FuncUpdate m
funcRetClean name name' (Func ty a) = do
    ( (ps,tys) , (a',ty') ) <- typeDrivenUnlambda ty a
    case ty' of
        -- This is just dumb, but whatever... works for the most common case
        -- TODO: nice generic algorithm
        TypeApp2 TypePair TypeUnit ty'' -> do
            let ty1 = tyfun tys ty''
                arity = length tys
                b' = AppOp1 OpSnd a'
                wrap = AppOp2 OpPair LitUnit
                upd = recmapped %~ tryApply (appRetClean name arity wrap name')
            return (upd, Func ty1 (lambda ps b'))
        TypeApp1 TypeTaint (TypeApp2 TypePair TypeUnit ty'') -> do
            let ty1 = tyfun tys (TypeApp1 TypeTaint ty'')
                arity = length tys
                b' = AppOp2 OpFmap (OpAccess OpSnd) a'
                wrap = AppOp2 OpFmap (AppOp1 OpPair LitUnit)
                upd = recmapped %~ tryApply (appRetClean name arity wrap name')
            return (upd, Func ty1 (lambda ps b'))
        _ -> mzero

appRetClean :: Name -> Int -> (Expression -> Expression) -> Name -> Expression -> Maybe Expression
appRetClean name arity wrap name' e
    | (Access op:es) <- unbeta e, op == name
    , arity == length es
    = Just $ wrap (beta (Access name':es))
appRetClean _ _ _ _ _ = Nothing
