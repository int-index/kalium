{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program.Scalar
	( module Sodium.Nucleus.Program.Scalar
	, module Sodium.Nucleus.Program
	) where

import Control.Lens
import Control.Lens.TH
import qualified Data.Map as M

import Sodium.Nucleus.Program

data Program = Program
    { _programFuncs :: [Func]
    }

data Func = Func
    { _funcSig :: FuncSig
    , _funcBody :: Body
    , _funcResult :: Expression
    }

data Body = Body
    { _bodyVars :: Vars
    , _bodyStatements :: [Statement]
    }

data Statement
    = Execute (Maybe Name) Name [Expression]
    | ForStatement ForCycle
    | MultiIfStatement MultiIf
    | BodyStatement Body

assign :: Name -> Expression -> Statement
assign name expr = Execute (Just name) (NameOp OpId) [expr]

data ForCycle
    = ForCycle
    { _forName :: Name
    , _forRange :: Expression
    , _forBody :: Body
    }

data MultiIf = MultiIf { _multiIfLeafs :: [(Expression, Body)] }

data Expression
    = Atom Atom
    | Call Name [Expression]

data Atom
    = Access Name
    | Primary Literal

bodyEmpty :: Body
bodyEmpty = Body M.empty []

makeLenses ''Func
makeLenses ''Body
makeLenses ''ForCycle
makeLenses ''MultiIf
makeLenses ''Program

makePrisms ''Statement
makePrisms ''Expression
makePrisms ''Atom

_Primary' :: Prism' Expression Literal
_Primary' = _Atom . _Primary

_Access' :: Prism' Expression Name
_Access'  = _Atom . _Access
