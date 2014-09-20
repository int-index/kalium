module Sodium.Nucleus.Side (side) where

import Control.Lens
import Control.Monad.Writer
import Control.Applicative
import Data.Either
import qualified Data.Map as M
import Sodium.Nucleus.Program.Scalar
import Sodium.Nucleus.Recmap.Scalar
import Data.Stack

type VarDecl = (Name, ClType)

side :: Program -> Program
side = flip evalStack (map NameGen [0..]) . recmapped sideStatement

sideStatement :: Statement -> Stack Name Statement
sideStatement = \case
    Assign name expr -> BodyStatement <$> sideAssign name expr
    MultiIfStatement multiIfBranch -> BodyStatement <$> sideMultiIf multiIfBranch
    Execute mname op exprs -> BodyStatement <$> sideExecute mname op exprs
    statement -> return statement

sideAssign :: Name -> Expression -> Stack Name Body
sideAssign name expr = do
    (e, xs) <- runWriterT (sideExpression expr)
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [SideCall name OpId [e]]
    return $ Body (M.fromList vardecls) statements

sideExpression
    :: Expression
    -> WriterT [Either VarDecl Statement] (Stack Name) Expression
sideExpression = \case
    Access name -> return (Access name)
    Primary lit -> return (Primary lit)
    Call op args -> do
        eArgs <- mapM sideExpression args
        name <- pop
        let vardecl = (name, ClVoid) -- TODO: the real type
        tell [Left vardecl]
        tell [Right $ SideCall name op eArgs]
        return (Access name)

sideMultiIf :: MultiIfBranch -> Stack Name Body
sideMultiIf multiIfBranch = do
    (leafs, xs) <- runWriterT (sideMultiIfLeafs $ view multiIfLeafs multiIfBranch)
    let (vardecls, assigns) = partitionEithers xs
    let statements = assigns ++ [MultiIfStatement $ set multiIfLeafs leafs multiIfBranch]
    return $ Body (M.fromList vardecls) statements

sideMultiIfLeafs
    :: [(Expression, Body)]
    -> WriterT [Either VarDecl Statement] (Stack Name) [(Expression, Body)]
sideMultiIfLeafs leafs = do
    forM leafs  $ \(expr, body) -> do
        name <- pop
        let vardecl = (name, ClVoid) -- TODO: the real type
        tell [Left vardecl]
        assign <- lift (sideAssign name expr)
        tell [Right $ BodyStatement assign]
        return (Access name, body)

sideExecute :: Maybe Name -> Operator -> [Expression] -> Stack Name Body
sideExecute mname op exprs = do
    (exprs', xs) <- runWriterT $ mapM sideExpression exprs
    let (vardecls, sidecalls) = partitionEithers xs
    let statements = sidecalls ++ [Execute mname op exprs']
    return $ Body (M.fromList vardecls) statements
