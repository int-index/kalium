{-# LANGUAGE TemplateHaskell #-}
module Sodium.Chloride.Program.Vector
	( module Sodium.Chloride.Program.Vector
	, module Sodium.Chloride.Program
	) where

import Control.Lens (prism', Simple, Prism)
import Control.Lens.TH
import qualified Data.Map as M

import Sodium.Chloride.Program

data Program
	= Program
	{ _programFuncs :: [Func]
	} deriving (Show)

data Func
	= Func
	{ _funcSig :: FuncSig
	, _funcBody :: Body
	} deriving (Show)

data Body
	= Body
	{ _bodyVars  :: Vars
	, _bodyBinds :: [Bind]
	, _bodyResults :: [Expression]
	} deriving (Show)

data Bind
	= Bind
	{ _bindIndices :: IndicesList
	, _bindStatement :: Statement
	} deriving (Show)

data Statement
	= Assign Expression
	| Execute Operator [Expression]
	| ForStatement ForCycle
	| MultiIfStatement MultiIfBranch
	| BodyStatement Body
	deriving (Show)

data ForCycle
	= ForCycle
	{ _forArgIndices :: IndicesList
	, _forArgExprs :: [Expression]
	, _forName :: Name
	, _forRange :: Expression
	, _forBody :: Body
	} deriving (Show)

data MultiIfBranch
	= MultiIfBranch
	{ _multiIfLeafs :: [(Expression, Body)]
	, _multiIfElse  :: Body
	} deriving (Show)

data Expression
	= Access Name Index
	| Fold Operator [Expression] Expression
	| Call Operator [Expression]
	| Primary Literal
	deriving (Eq, Show)

call OpId [arg] = arg
call op args = Call op args

data Index
	= Index Integer
	| Immutable
	| Uninitialized
	deriving (Eq, Show)

type Indices
	= M.Map Name Index

type IndicesList
	= [(Name, Index)]

makeLenses ''Func
makeLenses ''Bind
makeLenses ''Body
makeLenses ''ForCycle
makeLenses ''MultiIfBranch
makeLenses ''Program

makePrisms ''Statement
