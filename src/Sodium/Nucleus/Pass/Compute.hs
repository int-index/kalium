module Sodium.Nucleus.Pass.Compute (compute) where

import Control.Lens hiding (Index, Fold)
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector (recmapped)

compute :: Program -> Program
compute = over recmapped match

match :: Expression -> Expression
match = \case
    Call (NameOp OpId) [expr] -> expr
    Call (NameOp op) [x, y]
        | Primary (LitInteger a) <- x
        , Primary (LitInteger b) <- y
        , Just f <- binaryIntegerOp op
        -> Primary (LitInteger (f a b))
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

binaryIntegerOp :: Operator -> Maybe (Integer -> Integer -> Integer)
binaryIntegerOp = \case
    OpSubtract -> Just (-)
    OpAdd -> Just (+)
    OpMultiply -> Just (*)
    OpDiv -> Just div
    OpMod -> Just mod
    _ -> Nothing
