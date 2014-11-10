{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Sodium.Nucleus.Scalar.Typecheck where

import Data.Monoid
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map  as M

import Sodium.Nucleus.Scalar.Program

data TypeError
    = NoAccess Name Vars
    | NoFunction Name

declareLenses [d|

    data TypeScope param = TypeScope
        { tsFunctions :: M.Map Name (FuncSig param)
        , tsVariables :: Vars
        } deriving (Eq)

                |]

instance Monoid (TypeScope param) where
    mempty = TypeScope mempty mempty
    mappend (TypeScope funs1 vars1) (TypeScope funs2 vars2)
        = TypeScope (mappend funs1 funs2) (mappend vars1 vars2)

type TypeEnv param m = (Applicative m, MonadReader (TypeScope param) m, MonadError TypeError m)

class Typecheck param a where
    typecheck :: TypeEnv param m => a -> m Type

instance Typecheck param Literal where
    typecheck = return . typecheckLiteral

instance Typecheck param Atom where
    typecheck (Primary lit) = typecheck lit
    typecheck (Access name) = do
        vars <- asks (view tsVariables)
        M.lookup name vars
            & maybe (throwError $ NoAccess name vars) return

lookupFuncSig :: TypeEnv param m => Name -> m (FuncSig param)
lookupFuncSig name = do
    funcSigs <- asks (view tsFunctions)
    M.lookup name funcSigs
        & maybe (throwError (NoFunction name)) return

instance Typecheck param Expression where
    typecheck (Atom atom) = typecheck atom
    typecheck (Call name args)
        | NameOp op <- name = do
            mapM typecheck args >>= builtinOpType op
        | otherwise = funcSigType <$> lookupFuncSig name

builtinOpType :: TypeEnv param m => Operator -> [Type] -> m Type
builtinOpType _ _ = return TypeUnit


class TypeIntro param a where
    typeIntro' :: a -> TypeScope param -> TypeScope param

typeIntro :: (TypeIntro param a, TypeEnv param m) => (a -> m b) -> (a -> m b)
typeIntro k x = local (typeIntro' x) (k x)

instance TypeIntro param (Program param expr pat) where
    typeIntro' program = tsFunctions
        %~ mappend (program ^. programFuncs & M.map funcSig)

instance Scoping vars => TypeIntro param (Scope vars obj expr pat) where
    typeIntro' scope = tsVariables %~ mappend (scope ^. scopeVars . to scoping)
