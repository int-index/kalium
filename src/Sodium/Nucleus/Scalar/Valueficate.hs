{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Scalar.Valueficate where

import qualified Data.Map as M

import Control.Lens
import Control.Monad

import Sodium.Nucleus.Scalar.Program

instance Functor (Statement pat) where
    fmap f = \case
        Follow a1 a2 -> Follow (fmap f a1) (fmap f a2)
        ScopeStatement a -> ScopeStatement (fmap f a)
        IfStatement  a -> IfStatement  (fmap f a)
        ForStatement a -> ForStatement (fmap f a)
        Execute      a -> Execute      (fmap f a)
        Pass -> Pass

instance Functor (obj pat) => Functor (Scope vars obj pat) where
    fmap f = scopeElem %~ fmap f

instance Functor (If pat) where
    fmap f (If    ifCond          ifThen          ifElse)
          = If (f ifCond) (fmap f ifThen) (fmap f ifElse)

instance Functor (ForCycle pat) where
    fmap f (ForCycle forName    forRange          forStatement)
          = ForCycle forName (f forRange) (fmap f forStatement)

instance Functor (Exec pat) where
    fmap f = execArgs %~ map f

valueficate :: Program ByType Pattern Atom -> Program Type Pattern Expression
valueficate program =
    let referenceInfo = gather program
    in program & programFuncs %~ M.mapWithKey (valueficateFunc referenceInfo)

valueficateFunc
    :: ReferenceInfo
    -> Name
    -> Func ByType Pattern Atom
    -> Func   Type Pattern Expression
valueficateFunc referenceInfo name
    (Func ty (Scope params (Scope vars (Body statement result))))
    = let currentReferenceInfo = referenceInfo M.! name

          params' = params & map (\(name, (_by, ty)) -> (name, ty))

          ty' = typeTuple (ty : tys)
          result' = exprTuple (Atom result : map (Atom . Access) results)

          (results, tys) = unzip $ do
            (keep, param) <- zip currentReferenceInfo params'
            guard keep
            return param

          statement' = fmap Atom statement

      in Func ty' (Scope params' (Scope vars (Body statement' result')))

typeTuple :: [Type] -> Type
typeTuple = foldr1 TypePair

exprTuple :: [Expression] -> Expression
exprTuple = foldr1 (\x y -> Call (NameOp OpPair) [x, y])

type ReferenceInfo = M.Map Name [Bool]

gather :: Program ByType pat expr -> ReferenceInfo
gather = M.map inspect . view programFuncs where
    inspect = map check . view (funcScope . scopeVars)
    check (_name, (by, _ty)) = by == ByReference

{-
valueficate1 :: NameStack t m
             => Func ByType Pattern Atom -> m (Func Type Pattern Expression)
valueficate1 func = do
    let funcscope = func ^. funcScope
        funcscope1 = (funcscope & scopeVars . traversed . _2 %~ snd)
        type1 = typeTuple (func ^. funcType : map snd refs)
        refs = do
            (name, (by, ty)) <- funcscope ^. scopeVars
            guard (by == ByReference)
            return (name, ty)
    return $ Func type1 funcscope1
-}
