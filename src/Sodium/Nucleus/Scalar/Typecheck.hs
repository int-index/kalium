{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Sodium.Nucleus.Scalar.Typecheck where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M

import Sodium.Nucleus.Scalar.Program
import qualified Sodium.Nucleus.Scalar.Operator as Op

class Error e where
    errorNoAccess :: Name -> Vars -> e
    errorNoFunction :: Name -> e
    errorTypeMismatch :: Name -> [Type] -> e

declareLenses [d|

    data TypeScope = TypeScope
        { tsFunctions :: Map Name FuncSig
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
    typecheck = return . \case
        LitInteger _ -> TypeInteger
        LitDouble  _ -> TypeDouble
        LitChar    _ -> TypeChar

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
    typecheck (Call name tyArgs args) =
        case M.lookup name Op.operators of
            Just op -> do
                argTys <- traverse typecheck args
                let panic = throwError (errorTypeMismatch name argTys)
                maybe panic return (Op.tc op tyArgs argTys)
            Nothing -> funcSigType <$> lookupFuncSig name

class TypeIntro a where
    typeIntro' :: a -> TypeScope -> TypeScope

typeIntro :: (TypeIntro a, TypeEnv e m) => Endo' (Kleisli' m a b)
typeIntro k x = local (typeIntro' x) (k x)

instance Typing param => TypeIntro (Program param expr pat) where
    typeIntro' program = tsFunctions
        %~ mappend (program ^. programFuncs & fmap funcSig)

instance Scoping vars => TypeIntro (Scope vars obj expr pat) where
    typeIntro' scope = tsVariables %~ mappend (scope ^. scopeVars . to scoping)
