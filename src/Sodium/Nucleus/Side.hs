{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Sodium.Nucleus.Side (side) where

import Control.Lens
import Control.Monad.Writer
import Control.Applicative
import Data.Either
import qualified Data.Map as M
import Sodium.Nucleus.Program.Scalar
import Sodium.Nucleus.Recmap.Scalar
import Sodium.Nucleus.Name

type VarDecl = (Name, Type)

side :: NameStack t m => Program -> m Program
side = recmapped sideStatement

sideStatement :: NameStack t m => Statement -> m Statement
sideStatement = \case
    MultiIfStatement multiIf -> BodyStatement <$> sideMultiIf multiIf
    Execute mname op exprs -> BodyStatement <$> sideExecute mname op exprs
    ForStatement forCycle -> BodyStatement <$> sideForCycle forCycle
    statement -> return statement


sideExpression
    :: NameStack t m => Expression
    -> WriterT [Either VarDecl Statement] m Expression
sideExpression = \case
    Atom atom -> return (Atom atom)
    Call op args -> do
        eArgs <- mapM sideExpression args
        name <- namepop
        let vardecl = (name, TypeUnit) -- TODO: the real type
        tell [Left vardecl]
        tell [Right $ Execute (Just name) op eArgs]
        return (Atom (Access name))

sideMultiIf :: NameStack t m => MultiIf -> m Body
sideMultiIf multiIf = do
    (leafs, xs) <- runWriterT (mapM (_1 sideExpression) $ view multiIfLeafs multiIf)
    let (vardecls, assigns) = partitionEithers xs
    let statements = assigns ++ [MultiIfStatement $ set multiIfLeafs leafs multiIf]
    return $ Body (M.fromList vardecls) statements

sideExecute :: NameStack t m => Maybe Name -> Name -> [Expression] -> m Body
sideExecute mname op exprs = do
    (exprs', xs) <- runWriterT $ mapM sideExpression exprs
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [Execute mname op exprs']
    return $ Body (M.fromList vardecls) statements

sideForCycle :: NameStack t m => ForCycle -> m Body
sideForCycle forCycle = do
    (expr, xs) <- runWriterT (sideExpression $ view forRange forCycle)
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [ForStatement $ set forRange expr forCycle]
    return $ Body (M.fromList vardecls) statements
