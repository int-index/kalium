{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program.Scalar
	( module Sodium.Nucleus.Program.Scalar
	, module Sodium.Nucleus.Program
	) where

import Control.Lens
import qualified Data.Map as M

import Sodium.Nucleus.Program

data Program a = Program
    { _programFuncs :: [Func a]
    }

data FuncSig
    = FuncSig
    { _funcName :: Name
    , _funcParamTypes :: [ByType]
    , _funcRetType :: Type
    } deriving (Eq, Show)

data By
    = ByValue
    | ByReference
    deriving (Eq, Show)

type ByType = (By, Type)

data Func a = Func
    { _funcSig :: FuncSig
    , _funcParams :: [Name]
    , _funcBody :: Body a
    , _funcResult :: Atom
    }

data Body a = Body
    { _bodyVars :: Vars
    , _bodyStatements :: [Statement a]
    }

data Statement a
    = Execute (Exec a)
    | ForStatement (ForCycle a)
    | MultiIfStatement (MultiIf a)
    | BodyStatement (Body a)

assign :: Name -> a -> Statement a
assign name a = statement $ Exec (Just name) (NameOp OpId) [a]

class LiftStatement f where
    statement :: f a -> Statement a

instance LiftStatement Exec     where statement = Execute
instance LiftStatement ForCycle where statement = ForStatement
instance LiftStatement MultiIf  where statement = MultiIfStatement
instance LiftStatement Body     where statement = BodyStatement

data Exec a = Exec
    { _execRet :: Maybe Name
    , _execOp :: Name
    , _execArgs :: [a]
    }

data ForCycle a
    = ForCycle
    { _forName :: Name
    , _forRange :: a
    , _forBody :: Body a
    }

data MultiIf a = MultiIf { _multiIfLeafs :: [(a, Body a)] }

data Expression
    = Atom Atom
    | Call Name [Expression]

data Atom
    = Access Name
    | Primary Literal

bodyEmpty :: Body a
bodyEmpty = Body M.empty []

makeLenses ''FuncSig
makeLenses ''Func
makeLenses ''Body
makeLenses ''ForCycle
makeLenses ''MultiIf
makeLenses ''Program
makeLenses ''Exec

makePrisms ''Statement
makePrisms ''Expression
makePrisms ''Atom

_Primary' :: Prism' Expression Literal
_Primary' = _Atom . _Primary

_Access' :: Prism' Expression Name
_Access'  = _Atom . _Access
