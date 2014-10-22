{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
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

    data TypeScope = TypeScope
        { tsFunctions :: M.Map Name FuncSig
        , tsVariables :: Vars
        } deriving (Eq)

                |]

instance Monoid TypeScope where
    mempty = TypeScope mempty mempty
    mappend (TypeScope funs1 vars1) (TypeScope funs2 vars2)
        = TypeScope (mappend funs1 funs2) (mappend vars1 vars2)

type TypeEnv m = (Applicative m, MonadReader TypeScope m, MonadError TypeError m)

class Typecheck a where
    typecheck :: TypeEnv m => a -> m Type

typecheckLiteral :: Literal -> Type
typecheckLiteral = \case
    LitInteger _ -> TypeInteger
    LitDouble  _ -> TypeDouble
    LitBoolean _ -> TypeBoolean
    LitString  _ -> TypeString
    LitUnit      -> TypeUnit

instance Typecheck Literal where
    typecheck = return . typecheckLiteral

instance Typecheck Atom where
    typecheck (Primary lit) = typecheck lit
    typecheck (Access name) = do
        vars <- asks (view tsVariables)
        M.lookup name vars
            & maybe (throwError $ NoAccess name vars) return

lookupFuncSig :: TypeEnv m => Name -> m FuncSig
lookupFuncSig name = do
    funcSigs <- asks (view tsFunctions)
    M.lookup name funcSigs
        & maybe (throwError (NoFunction name)) return

instance Typecheck Expression where
    typecheck (Atom atom) = typecheck atom
    typecheck (Call name args)
        | NameOp op <- name = do
            mapM typecheck args >>= builtinOpType op
        | otherwise = funcSigType <$> lookupFuncSig name

builtinOpType :: TypeEnv m => Operator -> [Type] -> m Type
builtinOpType _ _ = return TypeUnit


class TypeIntro a where
    typeIntro' :: a -> TypeScope -> TypeScope

typeIntro :: (TypeIntro a, TypeEnv m) => (a -> m b) -> (a -> m b)
typeIntro k x = local (typeIntro' x) (k x)

instance TypeIntro (Program a) where
    typeIntro' program = tsFunctions
        %~ mappend (program ^. programFuncs & M.map funcSig)

instance TypeIntro (Func a) where
    typeIntro' func = tsVariables %~ mappend (funcParamVars func)
      where
        funcParamVars func
            = M.fromList (func ^. funcParams & map (_2 %~ snd))

instance TypeIntro (Scope v f a) where
    typeIntro' scope = tsVariables %~ mappend (scope ^. scopeVars . to (peeks id))
