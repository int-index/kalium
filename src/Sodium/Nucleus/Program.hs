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
	, _funcParams :: Vars
	, _funcRetType :: ClType
	} deriving (Show)

data Literal
	= INumber String
	| FNumber String String
	| ENumber String String Bool String
	| Quote String
	| BTrue
	| BFalse
	| Void
	deriving (Eq, Show)

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
	| OpReadLn ClType
	| OpId
	| OpName Name
	deriving (Eq, Show)

data ClType
	= ClInteger
	| ClDouble
	| ClBoolean
	| ClString
	| ClVoid
	deriving (Eq, Show)

type Vars
	= M.Map Name ClType

makeLenses ''FuncSig
