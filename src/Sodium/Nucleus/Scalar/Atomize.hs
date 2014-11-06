{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Sodium.Nucleus.Scalar.Atomize (atomize, atomize') where

import Data.Monoid
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative
import Sodium.Nucleus.Scalar.Program
import Sodium.Nucleus.Scalar.Build (statement)
import Sodium.Nucleus.Scalar.Typecheck
import Sodium.Nucleus.Name

type VarDecl = (Name, Type)

atomize' :: (MonadError TypeError m, NameStack t m) => Program Expression Pattern -> m (Program Atom Pattern)
atomize' program = runReaderT (atomize program) mempty

class Atomize a where
    atomize :: (TypeEnv m, NameStack t m) => a Expression Pattern -> m (a Atom Pattern)

instance Atomize Program where
    atomize = typeIntro $ programFuncs (traverse atomize)

instance Atomize Func where
    atomize = funcScope atomize

instance (Atomize f, Scoping v) => Atomize (Scope v f) where
    atomize = typeIntro $ scopeElem atomize

instance Atomize Body where
    atomize = bodyStatement atomize

instance Atomize Statement where

    atomize (Execute (Exec mname op exprs)) = atomizeStatement
        $ Exec mname op <$> mapM atomizeExpression exprs

    atomize (ForStatement (ForCycle name range body)) = atomizeStatement
        $ ForCycle name <$> atomizeExpression range <*> atomize body

    atomize (IfStatement (If cond thenb elseb)) = atomizeStatement
        $ If <$> atomizeExpression cond
             <*> atomize thenb
             <*> atomize elseb

    atomize (ScopeStatement scope) = ScopeStatement <$> atomize scope
    atomize (Group as) = Group <$> traverse atomize as

atomizeStatement w = do
    (a, (vardecls, statements)) <- runWriterT w
    return $ ScopeStatement
           $ Scope (scoping vardecls) (Group (statements `snoc` statement a))

atomizeExpression :: (TypeEnv m, NameStack t m) => Expression
                  -> WriterT ([VarDecl], [Statement Atom Pattern]) m Atom
atomizeExpression = \case
    Atom atom -> return atom
    e@(Call op args) -> do
        eArgs <- mapM atomizeExpression args
        name  <- namepop
        ty <- typecheck e
        let vardecl = (name, ty)
        tell ([vardecl], [Execute $ Exec (PAccess name) op eArgs])
        return (Access name)
