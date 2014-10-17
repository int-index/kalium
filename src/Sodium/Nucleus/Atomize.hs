{-# LANGUAGE ConstraintKinds #-}
module Sodium.Nucleus.Atomize (atomize) where

import Control.Lens
import Control.Monad.Writer
import Control.Applicative
import qualified Data.Map as M
import Sodium.Nucleus.Program.Scalar
import Sodium.Nucleus.Name

type VarDecl = (Name, Type)

class Atomize a where
    atomize :: NameStack t m => a Expression -> m (a Atom)

instance Atomize Program where atomize = (programFuncs . traversed) atomize
instance Atomize Func    where atomize = funcScope atomize
instance Atomize Scope   where atomize = scopeStatement atomize

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

atomizeExpression :: NameStack t m => Expression
                  -> WriterT ([VarDecl], [Statement Atom]) m Atom
atomizeExpression = \case
    Atom atom -> return atom
    Call op args -> do
        eArgs <- mapM atomizeExpression args
        name  <- namepop
        let vardecl = (name, TypeUnit) -- TODO: the real type
        tell ([vardecl], [Execute $ Exec (Just name) op eArgs])
        return (Access name)
