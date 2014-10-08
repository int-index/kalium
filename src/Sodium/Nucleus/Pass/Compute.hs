module Sodium.Nucleus.Pass.Compute (compute) where

import Control.Lens hiding (Index, Fold)
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector (recmapped)

compute :: Program -> Program
compute = over recmapped computeExpression

computeExpression :: Expression -> Expression
computeExpression = recursively match

recursively :: (Expression -> Expression) -> (Expression -> Expression)
recursively f = \case
    e@(Access _ _) -> f e
    e@(Primary  _) -> f e
    Call op exprs  -> f $ Call op (map rf exprs)
    Tuple   exprs  -> f $ Tuple (map rf exprs)
    Fold op expr range -> f $ Fold op (rf expr) (rf range)
    where rf = recursively f

match :: Expression -> Expression
match = \case
    Call (NameOp OpId) [expr] -> expr
    Tuple [expr] -> expr
    Tuple [    ] -> Primary LitUnit
    Fold (NameOp OpMultiply) (Primary (LitInteger 1)) range
        -> Call (NameOp OpProduct) [range]
    Fold (NameOp OpAdd)      (Primary (LitInteger 0)) range
        -> Call (NameOp OpSum)     [range]
    Fold (NameOp OpAnd) (Primary (LitBoolean True ))  range
        -> Call (NameOp OpAnd')    [range]
    Fold (NameOp OpOr)  (Primary (LitBoolean False))  range
        -> Call (NameOp OpOr')     [range]
    expr -> expr
