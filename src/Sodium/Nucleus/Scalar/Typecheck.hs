{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Sodium.Nucleus.Scalar.Typecheck where

import Data.Monoid
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map  as M

import Sodium.Nucleus.Scalar.Program

class Error e where
    errorNoAccess :: Name -> Vars -> e
    errorNoFunction :: Name -> e
    errorTypeMismatch :: Name -> [Type] -> e

declareLenses [d|

    data TypeScope = TypeScope
        { tsFunctions :: M.Map Name FuncSig
        , tsVariables :: Vars
        } deriving (Eq)

                |]

instance Monoid TypeScope where
    mempty = TypeScope mempty mempty
    mappend (TypeScope funs1 vars1) (TypeScope funs2 vars2)
        = TypeScope (mappend funs1 funs2) (mappend vars1 vars2)

type TypeEnv e m = (Applicative m, MonadReader TypeScope m, MonadError e m, Error e)

class Typecheck a where
    typecheck :: TypeEnv e m => a -> m Type

instance Typecheck Literal where
    typecheck = return . typecheckLiteral

instance Typecheck Atom where
    typecheck (Primary lit) = typecheck lit
    typecheck (Access name) = do
        vars <- asks (view tsVariables)
        M.lookup name vars
            & maybe (throwError $ errorNoAccess name vars) return

lookupFuncSig :: TypeEnv e m => Name -> m FuncSig
lookupFuncSig name = do
    funcSigs <- asks (view tsFunctions)
    M.lookup name funcSigs
        & maybe (throwError $ errorNoFunction name) return

instance Typecheck Expression where
    typecheck (Atom atom) = typecheck atom
    typecheck (Call name args)
        | NameOp op <- name = do
            mapM typecheck args >>= builtinOpType op
        | otherwise = funcSigType <$> lookupFuncSig name

builtinOpType :: TypeEnv e m => Operator -> [Type] -> m Type
builtinOpType op args = case op of
    OpAdd      -> argMatch2Same >>= require isNumeric
    OpSubtract -> argMatch2Same >>= require isNumeric
    OpMultiply -> argMatch2Same >>= require isNumeric
    OpDivide   -> argMatch2Same >>= require (==TypeDouble)
    OpDiv      -> argMatch2Same >>= require (==TypeInteger)
    OpMod      -> argMatch2Same >>= require (==TypeInteger)
    OpLess     -> argMatch2Same >>= require isOrd >> return TypeBoolean
    OpMore     -> argMatch2Same >>= require isOrd >> return TypeBoolean
    OpEquals   -> argMatch2Same >>= require isEq  >> return TypeBoolean
    OpAnd      -> argMatch2Same >>= require (==TypeBoolean)
    OpOr       -> argMatch2Same >>= require (==TypeBoolean)
    OpNot      -> argMatch1 >>= require (==TypeBoolean)
    OpXor      -> argMatch2Same >>= require (==TypeBoolean)
    OpRange    -> TypeList <$> argMatch2Same
    OpElem     -> do
        (ty, tys) <- argMatch2
        if tys == TypeList ty
            then return TypeBoolean
            else panic
    OpShow     -> return (TypeList TypeChar)
    OpNegate   -> argMatch1 >>= require isNumeric
    OpPrintLn  -> argMatch1 >> return TypeUnit
    OpReadLn   -> return TypeUnit -- TODO: type inference?
    OpPutLn    -> argMatch1 >>= require (==TypeList TypeChar) >> return TypeUnit
    OpGetLn    -> return (TypeList TypeChar)
    OpId       -> argMatch1
    OpPair     -> uncurry TypePair <$> argMatch2
    OpFst      -> fst <$> argMatch2
    OpSnd      -> snd <$> argMatch2
    OpSingleton-> TypeList <$> argMatch1
    OpConcat   -> argMatch2Same >>= require isList
    OpIntToDouble -> argMatch1 >>= require (==TypeInteger) >> return TypeDouble
    OpMain        -> return TypeUnit
    _ -> error ("Not available: " ++ show op)
    where isNumeric ty = ty == TypeInteger || ty == TypeDouble
          isEq  _ty = True -- for now
          isOrd _ty = True -- ^^
          isList = \case
            TypeList _ -> True
            _ -> False
          argMatch1 = case args of
            [ty] -> return ty
            _ -> panic
          argMatch2 = case args of
            [ty1, ty2] -> return (ty1, ty2)
            _ -> panic
          argMatch2Same = argMatch2 >>= \case
            (ty1, ty2) | ty1 == ty2 -> return ty1
            _ -> panic
          require p ty | p ty = return ty
                       | otherwise = panic
          panic :: TypeEnv e m => m a
          panic = throwError (errorTypeMismatch (NameOp op) args)

class TypeIntro a where
    typeIntro' :: a -> TypeScope -> TypeScope

typeIntro :: (TypeIntro a, TypeEnv e m) => (a -> m b) -> (a -> m b)
typeIntro k x = local (typeIntro' x) (k x)

instance Typing param => TypeIntro (Program param expr pat) where
    typeIntro' program = tsFunctions
        %~ mappend (program ^. programFuncs & M.map funcSig)

instance Scoping vars => TypeIntro (Scope vars obj expr pat) where
    typeIntro' scope = tsVariables %~ mappend (scope ^. scopeVars . to scoping)
