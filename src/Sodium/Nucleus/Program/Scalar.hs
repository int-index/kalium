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
    , _funcScope :: Scope a
    , _funcResult :: Atom
    }

data Statement a
    = Execute (Exec a)
    | ForStatement (ForCycle a)
    | IfStatement (If a)
    | ScopeStatement (Scope a)
    | Group [Statement a]

assign :: Name -> a -> Statement a
assign name a = statement $ Exec (Just name) (NameOp OpId) [a]

class LiftStatement f where
    statement :: f a -> Statement a

instance LiftStatement Exec     where statement = Execute
instance LiftStatement ForCycle where statement = ForStatement
instance LiftStatement If       where statement = IfStatement
instance LiftStatement Scope    where statement = ScopeStatement

data Exec a = Exec
    { _execRet :: Maybe Name
    , _execOp :: Name
    , _execArgs :: [a]
    }

data ForCycle a
    = ForCycle
    { _forName :: Name
    , _forRange :: a
    , _forStatement :: Statement a
    }

data If a = If
    { _ifCond :: a
    , _ifThen :: Statement a
    , _ifElse :: Statement a
    }

data Scope a = Scope
    { _scopeVars :: Vars
    , _scopeStatement :: Statement a
    }

data Expression
    = Atom Atom
    | Call Name [Expression]

data Atom
    = Access Name
    | Primary Literal

makeLenses ''FuncSig
makeLenses ''Func
makeLenses ''Scope
makeLenses ''ForCycle
makeLenses ''If
makeLenses ''Program
makeLenses ''Exec

makePrisms ''Statement
makePrisms ''Expression
makePrisms ''Atom

_Primary' :: Prism' Expression Literal
_Primary' = _Atom . _Primary

_Access' :: Prism' Expression Name
_Access'  = _Atom . _Access
