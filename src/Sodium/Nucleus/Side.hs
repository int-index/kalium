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

type NameStack = State [Name]
pop = gets head <* modify tail

side :: Program -> Program
side = flip evalState (map NameGen [0..]) . recmapped sideStatement

sideStatement :: Statement -> NameStack Statement
sideStatement = \case
    Assign name expr -> BodyStatement <$> sideAssign name expr
    MultiIfStatement multiIfBranch -> BodyStatement <$> sideMultiIf multiIfBranch
    Execute mname op exprs -> BodyStatement <$> sideExecute mname op exprs
    ForStatement forCycle -> BodyStatement <$> sideForCycle forCycle
    statement -> return statement

sideAssign :: Name -> Expression -> NameStack Body
sideAssign name expr = do
    (e, xs) <- runWriterT (sideExpression expr)
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [Execute (Just name) (NameOp OpId) [e]]
    return $ Body (M.fromList vardecls) statements


sideExpression
    :: Expression
    -> WriterT [Either VarDecl Statement] NameStack Expression
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

sideMultiIf :: MultiIfBranch -> NameStack Body
sideMultiIf multiIfBranch = do
    (leafs, xs) <- runWriterT (mapM (_1 sideExpression) $ view multiIfLeafs multiIfBranch)
    let (vardecls, assigns) = partitionEithers xs
    let statements = assigns ++ [MultiIfStatement $ set multiIfLeafs leafs multiIfBranch]
    return $ Body (M.fromList vardecls) statements

sideExecute :: Maybe Name -> Name -> [Expression] -> NameStack Body
sideExecute mname op exprs = do
    (exprs', xs) <- runWriterT $ mapM sideExpression exprs
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [Execute mname op exprs']
    return $ Body (M.fromList vardecls) statements

sideForCycle :: ForCycle -> NameStack Body
sideForCycle forCycle = do
    (expr, xs) <- runWriterT (sideExpression $ view forRange forCycle)
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [ForStatement $ set forRange expr forCycle]
    return $ Body (M.fromList vardecls) statements
