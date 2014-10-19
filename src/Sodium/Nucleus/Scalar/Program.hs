{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Scalar.Program
    ( module Sodium.Nucleus.Scalar.Program
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
    { _scopeVars :: M.Map Name Type
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
