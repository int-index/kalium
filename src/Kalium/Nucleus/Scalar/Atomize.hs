{-# LANGUAGE FlexibleContexts #-}
module Kalium.Nucleus.Scalar.Atomize (atomize) where

import Kalium.Prelude
import Kalium.Util

import qualified Data.Map as M

import Kalium.Nucleus.Scalar.Program
import Kalium.Nucleus.Scalar.Build (statement, follow)
import Kalium.Nucleus.Scalar.Typecheck

atomize a = runReaderT (atomizeProgram a) mempty

type T e m = (TypeEnv e m, MonadNameGen m)
type Atomize a = forall e m param . (Typing param, T e m)
    => Kleisli' m
        (a (Configuration param Pattern Expression))
        (a (Configuration param Pattern Atom))

atomizeProgram :: Atomize Program
atomizeProgram = typeIntro $ programFuncs (traverse atomizeFunc)

atomizeFunc :: Atomize Func
atomizeFunc = funcScope (atomizeScope atomizeBodyScope)

atomizeBodyScope :: Atomize (Scope Vars Body)
atomizeBodyScope = typeIntro $ \(Scope vars (Body stmt expr)) -> do
    (atom, (vardecls, statements)) <- runWriterT (atomizeExpression expr)
    statement <- atomizeStatement stmt
    return $ Scope
        (M.fromList vardecls <> vars)
        (Body (follow (statement:statements)) atom)

atomizeScope atomize = typeIntro $ scopeElem atomize

atomizeStatement :: Atomize Statement
atomizeStatement = \case
    Execute (Exec mname op tyArgs exprs) -> atomizeStatementW
        $ Exec mname op tyArgs <$> traverse atomizeExpression exprs

    ForStatement (ForCycle name range body) -> atomizeStatementW
        $ ForCycle name <$> atomizeExpression range <*> atomizeStatement body

    IfStatement (If cond thenb elseb) -> atomizeStatementW
        $ If <$> atomizeExpression cond
             <*> atomizeStatement thenb
             <*> atomizeStatement elseb

    ScopeStatement scope
         -> ScopeStatement
        <$> atomizeScope atomizeStatement scope

    Follow st1 st2
         -> Follow
        <$> atomizeStatement st1
        <*> atomizeStatement st2

    Pass -> pure Pass

atomizeStatementW w = do
    (a, (vardecls, statements)) <- runWriterT w
    return $ ScopeStatement
           $ Scope (scoping vardecls) (follow (statements `snoc` statement a))

atomizeExpression
    :: T e m
    => Expression
    -> WriterT
        ( Pairs Name Type
        , [Statement (Configuration param Pattern Atom)]
        ) m Atom
atomizeExpression = \case
    Atom atom -> return atom
    e@(Call op tyArgs args) -> do
        eArgs <- traverse atomizeExpression args
        name  <- NameGen <$> mkname Nothing
        ty <- typecheck e
        let vardecl = (name, ty)
        tell ([vardecl], [Execute $ Exec (PAccess name) op tyArgs eArgs])
        return (Access name)
