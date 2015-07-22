{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Kalium.Nucleus.Scalar.Typecheck where

import Kalium.Prelude
import Kalium.Util

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

import Kalium.Nucleus.Scalar.Program
import qualified Kalium.Nucleus.Scalar.Operator as Op

instance Exception ErrorNoAccess
data ErrorNoAccess = ErrorNoAccess Name Vars
    deriving (Show)

instance Exception ErrorNoFunction
data ErrorNoFunction = ErrorNoFunction Name
    deriving (Show)

instance Exception ErrorTypeMismatch
data ErrorTypeMismatch = ErrorTypeMismatch Name [Type]
    deriving (Show)

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

type TypeEnv e m = (MonadReader TypeScope m, MonadError SomeException m)

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
        M.lookup name vars & (throwMaybe.SomeException) (ErrorNoAccess name vars)

lookupFuncSig :: TypeEnv e m => Name -> m FuncSig
lookupFuncSig name = do
    funcSigs <- asks (view tsFunctions)
    M.lookup name funcSigs & (throwMaybe.SomeException) (ErrorNoFunction name)

instance Typecheck Expression where
    typecheck (Atom atom) = typecheck atom
    typecheck (Call name tyArgs args) =
        case M.lookup name Op.operators of
            Just op -> do
                argTys <- traverse typecheck args
                Op.tc op tyArgs argTys
                    & (throwMaybe.SomeException)
                      (ErrorTypeMismatch name argTys)
            Nothing -> funcSigType <$> lookupFuncSig name

class TypeIntro a where
    typeIntro' :: a -> TypeScope -> TypeScope

typeIntro :: (TypeIntro a, TypeEnv e m) => Endo' (Kleisli' m a b)
typeIntro k x = local (typeIntro' x) (k x)

instance Typing (GetParameter config) => TypeIntro (Program config) where
    typeIntro' program = tsFunctions
        %~ mappend (program ^. programFuncs & fmap funcSig)

instance Scoping vars => TypeIntro (Scope vars obj config) where
    typeIntro' scope = tsVariables %~ mappend (scope ^. scopeVars . to scoping)
