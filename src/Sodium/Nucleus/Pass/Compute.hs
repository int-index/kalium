{-# LANGUAGE GADTs #-}
module Sodium.Nucleus.Pass.Compute (compute) where

import Control.Lens hiding (Index, Fold)
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector (recmapped)

compute :: Program -> Program
compute = over recmapped match

match :: Expression -> Expression
match = \case
    Call (NameOp OpId) [expr] -> expr
    Call (NameOp op) [x]
        | Primary (DLit STypeInteger a) <- x
        , Just f <- unaryIntegerOp op
        -> Primary (DLit STypeInteger (f a))
    Call (NameOp op) [x, y]
        | Primary (DLit STypeInteger a) <- x
        , Primary (DLit STypeInteger b) <- y
        , Just f <- binaryIntegerOp op
        -> Primary (DLit STypeInteger (f a b))
    Tuple [expr] -> expr
    Tuple [    ] -> Primary (DLit STypeUnit ())
    Fold (NameOp OpMultiply) (Primary (DLit STypeInteger 1)) range
        -> Call (NameOp OpProduct) [range]
    Fold (NameOp OpAdd)      (Primary (DLit STypeInteger 0)) range
        -> Call (NameOp OpSum)     [range]
    Fold (NameOp OpAnd) (Primary (DLit STypeBoolean True ))  range
        -> Call (NameOp OpAnd')    [range]
    Fold (NameOp OpOr)  (Primary (DLit STypeBoolean False))  range
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

unaryIntegerOp :: Operator -> Maybe (Integer -> Integer)
unaryIntegerOp = \case
    OpNegate -> Just negate
    _ -> Nothing
