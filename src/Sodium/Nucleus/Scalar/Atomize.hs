{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

atomize' :: (MonadError TypeError m, NameStack t m) => Program ByType Pattern Expression -> m (Program ByType Pattern Atom)
atomize' program = runReaderT (atomize program) mempty

class Atomize a where
    atomize :: (TypeEnv ByType m, NameStack t m) => a Expression -> m (a Atom)

instance Atomize (Program ByType Pattern) where
    atomize = typeIntro $ programFuncs (traverse atomize)

instance Atomize (Func ByType Pattern) where
    atomize = funcScope atomize

instance (Atomize (obj pat), Scoping vars) => Atomize (Scope vars obj pat) where
    atomize = typeIntro $ scopeElem atomize

instance Atomize (Body Pattern) where
    atomize = bodyStatement atomize

instance Atomize (Statement Pattern) where

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

atomizeExpression :: (TypeEnv param m, NameStack t m) => Expression
                  -> WriterT ([VarDecl], [Statement Pattern Atom]) m Atom
atomizeExpression = \case
    Atom atom -> return atom
    e@(Call op args) -> do
        eArgs <- mapM atomizeExpression args
        name  <- namepop
        ty <- typecheck e
        let vardecl = (name, ty)
        tell ([vardecl], [Execute $ Exec (PAccess name) op eArgs])
        return (Access name)
