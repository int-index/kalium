{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program where

import Control.Lens.TH
import qualified Data.Map as M

data Name
    = Name String
    | NameMain
    | NameGen Integer
    | NameUnique Name
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
    | OpLess
    | OpMore
    | OpEquals
    | OpAnd
    | OpOr
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
    | OpName Name
    deriving (Eq, Show)

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
    deriving (Eq, Show)

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
