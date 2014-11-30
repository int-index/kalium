{-# LANGUAGE GADTs #-}
module Sodium.Nucleus.Pass.Compute (compute) where

import Control.Lens hiding (Index, Fold)
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector (recmapped)

compute :: Program -> Program
compute = over recmapped match

match :: Expression -> Expression
match = \case
    Call (OpAccess OpId) expr -> expr
    Call (OpAccess OpSingleton) (Primary (Lit t a)) -> Primary (Lit (STypeList t) [a])
    Call (OpAccess op) x
        | Primary (Lit STypeInteger a) <- x
        , Just f <- unaryIntegerOp op
        -> Primary (Lit STypeInteger (f a))
    Call2 (OpAccess op) x y
        | Primary (Lit STypeInteger a) <- x
        , Primary (Lit STypeInteger b) <- y
        , Just f <- binaryIntegerOp op
        -> Primary (Lit STypeInteger (f a b))
    Call (OpAccess OpFst) (Call2 (OpAccess OpPair) expr1 _) -> expr1
    Call (OpAccess OpSnd) (Call2 (OpAccess OpPair) _ expr2) -> expr2
    Call3 (OpAccess OpFold) (OpAccess OpMultiply) (Primary (Lit STypeInteger 1)) range
        -> Call (OpAccess OpProduct) range
    Call3 (OpAccess OpFold) (OpAccess OpAdd) (Primary (Lit STypeInteger 0)) range
        -> Call (OpAccess OpSum)     range
    Call3 (OpAccess OpFold) (OpAccess OpAnd) (Primary (Lit STypeBoolean True)) range
        -> Call (OpAccess OpAnd')    range
    Call3 (OpAccess OpFold) (OpAccess OpOr) (Primary (Lit STypeBoolean False)) range
        -> Call (OpAccess OpOr')     range
    Call (OpAccess OpPutLn) (Call (OpAccess OpShow) expr) -> Call (OpAccess OpPrintLn) expr
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
