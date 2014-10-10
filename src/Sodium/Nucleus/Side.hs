{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Sodium.Nucleus.Side (side) where

import Control.Lens
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative
import Data.Either
import qualified Data.Map as M
import Sodium.Nucleus.Program.Scalar
import Sodium.Nucleus.Recmap.Scalar

type VarDecl = (Name, Type)
type Stack s m = (Applicative m, MonadState [s] m)

pop :: Stack Name m => m Name
pop = gets head <* modify tail

side :: Stack Name m => Program -> m Program
side = recmapped sideStatement

sideStatement :: Stack Name m => Statement -> m Statement
sideStatement = \case
    Assign name expr -> BodyStatement <$> sideAssign name expr
    MultiIfStatement multiIf -> BodyStatement <$> sideMultiIf multiIf
    Execute mname op exprs -> BodyStatement <$> sideExecute mname op exprs
    ForStatement forCycle -> BodyStatement <$> sideForCycle forCycle
    statement -> return statement

sideAssign :: Stack Name m => Name -> Expression -> m Body
sideAssign name expr = do
    (e, xs) <- runWriterT (sideExpression expr)
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [Execute (Just name) (NameOp OpId) [e]]
    return $ Body (M.fromList vardecls) statements


sideExpression
    :: Stack Name m => Expression
    -> WriterT [Either VarDecl Statement] m Expression
sideExpression = \case
    Access name -> return (Access name)
    Primary lit -> return (Primary lit)
    Call op args -> do
        eArgs <- mapM sideExpression args
        name <- pop
        let vardecl = (name, TypeUnit) -- TODO: the real type
        tell [Left vardecl]
        tell [Right $ Execute (Just name) op eArgs]
        return (Access name)

sideMultiIf :: Stack Name m => MultiIf -> m Body
sideMultiIf multiIf = do
    (leafs, xs) <- runWriterT (mapM (_1 sideExpression) $ view multiIfLeafs multiIf)
    let (vardecls, assigns) = partitionEithers xs
    let statements = assigns ++ [MultiIfStatement $ set multiIfLeafs leafs multiIf]
    return $ Body (M.fromList vardecls) statements

sideExecute :: Stack Name m => Maybe Name -> Name -> [Expression] -> m Body
sideExecute mname op exprs = do
    (exprs', xs) <- runWriterT $ mapM sideExpression exprs
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [Execute mname op exprs']
    return $ Body (M.fromList vardecls) statements

sideForCycle :: Stack Name m => ForCycle -> m Body
sideForCycle forCycle = do
    (expr, xs) <- runWriterT (sideExpression $ view forRange forCycle)
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [ForStatement $ set forRange expr forCycle]
    return $ Body (M.fromList vardecls) statements
