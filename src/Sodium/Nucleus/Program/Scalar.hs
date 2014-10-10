{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program.Scalar
	( module Sodium.Nucleus.Program.Scalar
	, module Sodium.Nucleus.Program
	) where

import Control.Lens (prism', Simple, Prism)
import Control.Lens.TH
import qualified Data.Map as M

import Sodium.Nucleus.Program

data Program
	= Program
	{ _programFuncs :: [Func]
	} deriving (Show)

data Func
	= Func
	{ _funcSig :: FuncSig
	, _funcBody :: Body
	, _funcResults :: [Expression]
	} deriving (Show)

data Body
	= Body
	{ _bodyVars :: Vars
	, _bodyStatements :: [Statement]
	} deriving (Show)

data Statement
	= Assign Name Expression
	| Execute (Maybe Name) Name [Expression]
	| ForStatement ForCycle
	| MultiIfStatement MultiIf
	| BodyStatement Body
	deriving (Show)

data ForCycle
	= ForCycle
	{ _forName :: Name
	, _forRange :: Expression
	, _forBody :: Body
	} deriving (Show)

data MultiIf
	= MultiIf
	{ _multiIfLeafs :: [(Expression, Body)]
	, _multiIfElse  :: Body
	} deriving (Show)

data Expression
	= Access Name
	| Call Name [Expression]
	| Primary Literal
	deriving (Show)

bodyEmpty :: Body
bodyEmpty = Body M.empty []

bodySingleton :: Simple Prism Body Statement
bodySingleton
	= prism' (\s -> Body M.empty [s])
	$ \case
		Body vars [statement] | M.null vars -> Just statement
		_ -> Nothing

makeLenses ''Func
makeLenses ''Body
makeLenses ''ForCycle
makeLenses ''MultiIf
makeLenses ''Program

makePrisms ''Statement
