{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Sodium.Nucleus.Scalar.Atomize (atomize, atomize') where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M

import Sodium.Nucleus.Scalar.Program
import Sodium.Nucleus.Scalar.Build (statement, follow)
import Sodium.Nucleus.Scalar.Typecheck

atomize' :: (Atomize a, MonadError e m, Error e, MonadSupply Integer m, Applicative m)
         => a Expression -> m (a Atom)
atomize' a = runReaderT (atomize a) mempty

class Atomize a where
    atomize :: (TypeEnv e m, MonadSupply Integer m) => a Expression -> m (a Atom)

instance Typing param => Atomize (Program param Pattern) where
    atomize = typeIntro $ programFuncs (traverse atomize)

instance Typing param => Atomize (Func param Pattern) where
    atomize = funcScope atomize

instance Atomize (Scope Vars Body Pattern) where
    atomize = typeIntro $ \(Scope vars (Body stmt expr)) -> do
        (atom, (vardecls, statements)) <- runWriterT (atomizeExpression expr)
        statement <- atomize stmt
        return $ Scope
            (M.fromList vardecls <> vars)
            (Body (follow (statement:statements)) atom)

instance (Atomize (obj pat), Scoping vars) => Atomize (Scope vars obj pat) where
    atomize = typeIntro $ scopeElem atomize

instance Atomize (Statement Pattern) where

    atomize (Execute (Exec mname op tyArgs exprs)) = atomizeStatement
        $ Exec mname op tyArgs <$> traverse atomizeExpression exprs

    atomize (ForStatement (ForCycle name range body)) = atomizeStatement
        $ ForCycle name <$> atomizeExpression range <*> atomize body

    atomize (IfStatement (If cond thenb elseb)) = atomizeStatement
        $ If <$> atomizeExpression cond
             <*> atomize thenb
             <*> atomize elseb

    atomize (ScopeStatement scope) = ScopeStatement <$> atomize scope
    atomize (Follow st1 st2) = Follow <$> atomize st1 <*> atomize st2
    atomize Pass = pure Pass

atomizeStatement w = do
    (a, (vardecls, statements)) <- runWriterT w
    return $ ScopeStatement
           $ Scope (scoping vardecls) (follow (statements `snoc` statement a))

atomizeExpression :: (TypeEnv e m, MonadSupply Integer m) => Expression
                  -> WriterT (Pairs Name Type, [Statement Pattern Atom]) m Atom
atomizeExpression = \case
    Atom atom -> return atom
    e@(Call op tyArgs args) -> do
        eArgs <- traverse atomizeExpression args
        name  <- NameGen <$> supply
        ty <- typecheck e
        let vardecl = (name, ty)
        tell ([vardecl], [Execute $ Exec (PAccess name) op tyArgs eArgs])
        return (Access name)
