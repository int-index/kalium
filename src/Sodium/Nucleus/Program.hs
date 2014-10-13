{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program where

import qualified Data.Map as M

type NameSpace = [String]

data Name
    = Name NameSpace String
    | NameMain
    | NameOp Operator
    | Shadow Name
    deriving (Eq, Ord, Show)

data Operator
    = OpAdd
    | OpSubtract
    | OpMultiply
    | OpDivide
    | OpDiv
    | OpMod
    | OpLess
    | OpMore
    | OpEquals
    | OpAnd
    | OpOr
    | OpNot
    | OpXor
    | OpRange
    | OpElem
    | OpShow
    | OpNegate
    | OpProduct
    | OpSum
    | OpAnd'
    | OpOr'
    | OpPrintLn
    | OpReadLn Type
    | OpId
    | OpFst
    | OpSnd
    deriving (Eq, Ord, Show)

data Literal
    = LitInteger Integer
    | LitDouble  Rational
    | LitBoolean Bool
    | LitString  String
    | LitUnit
    deriving (Eq, Show)

data Type
    = TypeInteger
    | TypeDouble
    | TypeBoolean
    | TypeString
    | TypeUnit
    deriving (Eq, Ord, Show)

type Vars
    = M.Map Name Type
