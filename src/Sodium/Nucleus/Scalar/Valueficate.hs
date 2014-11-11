{-# LANGUAGE ConstraintKinds #-}
module Sodium.Nucleus.Scalar.Valueficate where

import Control.Lens
import Control.Monad

import Sodium.Nucleus.Scalar.Program
import Sodium.Nucleus.Name

valueficate :: NameStack t m
            => Program ByType Pattern Atom -> m (Program Type Pattern Atom)
valueficate = (programFuncs . traversed) valueficate1

typeTuple :: [Type] -> Type
typeTuple [ ] = TypeUnit
typeTuple  ts = foldr1 TypePair ts

valueficate1 :: NameStack t m
             => Func ByType Pattern Atom -> m (Func Type Pattern Atom)
valueficate1 func = do
    let funcscope = func ^. funcScope
        funcscope1 = (funcscope & scopeVars . traversed . _2 %~ snd)
        type1 = typeTuple (func ^. funcType : map snd refs)
        refs = do
            (name, (by, ty)) <- funcscope ^. scopeVars
            guard (by == ByReference)
            return (name, ty)
    return $ Func type1 funcscope1
