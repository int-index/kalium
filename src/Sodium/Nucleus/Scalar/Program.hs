{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Sodium.Nucleus.Scalar.Program
    ( module Sodium.Nucleus.Scalar.Program
    , module Sodium.Nucleus.Program
    ) where

import Control.Lens
import qualified Data.Map as M

import Sodium.Nucleus.Program
import Sodium.Util

class Typing t where
    typing :: t -> Type

instance Typing Type   where typing = id
instance Typing ByType where typing = snd

class Scoping vars where
    scoping :: vars -> M.Map Name Type

instance Typing t => Scoping (M.Map Name t) where
    scoping = M.map typing
instance Typing t => Scoping (Pairs Name t) where
    scoping = scoping . M.fromList

data Program param expr pat = Program
    { _programFuncs :: M.Map Name (Func param expr pat)
    }

data By
    = ByValue
    | ByReference
    deriving (Eq, Show)

type ByType = (By, Type)

data Func param expr pat = Func
    { _funcType :: Type
    , _funcScope :: Scope (Pairs Name param) (Scope Vars Body) expr pat
    }

data Body expr pat = Body
    { _bodyStatement :: Statement expr pat
    , _bodyResult :: Atom
    }

data Statement expr pat
    = Execute (Exec expr pat)
    | ForStatement (ForCycle expr pat)
    | IfStatement (If expr pat)
    | forall vars . Scoping vars => ScopeStatement (Scope vars Statement expr pat)
    | Group [Statement expr pat]

data Pattern
    = PUnit
    | PAccess Name

data Exec expr pat = Exec
    { _execRet :: pat
    , _execOp :: Name
    , _execArgs :: [expr]
    }

data ForCycle expr pat
    = ForCycle
    { _forName :: Name
    , _forRange :: expr
    , _forStatement :: Statement expr pat
    }

data If expr pat = If
    { _ifCond :: expr
    , _ifThen :: Statement expr pat
    , _ifElse :: Statement expr pat
    }

data Scope vars obj expr pat = Scope
    { _scopeVars :: vars
    , _scopeElem :: obj expr pat
    }

data Expression
    = Atom Atom
    | Call Name [Expression]

data Atom
    = Access Name
    | Primary Literal

makeLenses ''Func
makeLenses ''Body
makeLenses ''Scope
makeLenses ''ForCycle
makeLenses ''If
makeLenses ''Program
makeLenses ''Exec

data FuncSig param = FuncSig
    { funcSigType :: Type
    , funcSigParamTypes :: [param]
    } deriving (Eq)

funcSig :: Func param a p -> FuncSig param
funcSig func = FuncSig (func ^. funcType) (func ^. funcScope . scopeVars & map snd)
