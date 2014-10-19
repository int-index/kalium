{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Sodium.Nucleus.Scalar.Atomize (atomize, atomize') where

import Data.Monoid
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as M
import Sodium.Nucleus.Scalar.Program
import Sodium.Nucleus.Scalar.Build (statement)
import Sodium.Nucleus.Scalar.Typecheck
import Sodium.Nucleus.Name

type VarDecl = (Name, Type)

atomize' :: (MonadError TypeError m, NameStack t m) => Program Expression -> m (Program Atom)
atomize' program = runReaderT (atomize program) mempty

class Atomize a where
    atomize :: (TypeEnv m, NameStack t m) => a Expression -> m (a Atom)

instance Atomize Program where
    atomize = typeIntro $ programFuncs (traverse atomize)

instance Atomize Func where
    atomize = typeIntro $ funcScope atomize

instance Atomize Scope where
    atomize = typeIntro $ scopeStatement atomize

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
           $ Scope (M.fromList vardecls) (Group (statements `snoc` statement a))

atomizeExpression :: (TypeEnv m, NameStack t m) => Expression
                  -> WriterT ([VarDecl], [Statement Atom]) m Atom
atomizeExpression = \case
    Atom atom -> return atom
    e@(Call op args) -> do
        eArgs <- mapM atomizeExpression args
        name  <- namepop
        ty <- typecheck e
        let vardecl = (name, ty)
        tell ([vardecl], [Execute $ Exec (Just name) op eArgs])
        return (Access name)
