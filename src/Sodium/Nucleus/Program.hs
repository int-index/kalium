{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program where

import Control.Lens.TH
import qualified Data.Map as M

type NameSpace = [String]

data Name
    = Name NameSpace String
    | NameMain
    | NameGen Integer
    | NameOp Operator
    deriving (Eq, Ord, Show)

data FuncSig
    = FuncSig
    { _funcName :: Name
    , _funcParams  :: Params
    , _funcRetType :: Type
    } deriving (Eq, Show)

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

data By
    = ByValue
    | ByReference
    deriving (Eq, Show)

type ByType = (By, Type)

type Vars
    = M.Map Name Type

type Params
    = [(Name, ByType)]

makeLenses ''FuncSig
