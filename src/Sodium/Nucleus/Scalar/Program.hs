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

class Typing t where
    typing :: t -> Type

instance Typing Type   where typing = id
instance Typing ByType where typing = snd

class Scoping v where
    scoping :: v -> M.Map Name Type

instance Typing t => Scoping (M.Map Name t) where
    scoping = M.map typing
instance Typing t => Scoping [(Name, t)] where
    scoping = scoping . M.fromList

data Program a = Program
    { _programFuncs :: M.Map Name (Func a)
    }

data By
    = ByValue
    | ByReference
    deriving (Eq, Show)

type ByType = (By, Type)

type Params = [(Name, ByType)]

data Func a = Func
    { _funcType :: Type
    , _funcScope :: Scope Params (Scope Vars Body) a
    }

data Body a = Body
    { _bodyStatement :: Statement a
    , _bodyResult :: Atom
    }

data Statement a
    = Execute (Exec a)
    | ForStatement (ForCycle a)
    | IfStatement (If a)
    | forall v . Scoping v => ScopeStatement (Scope v Statement a)
    | Group [Statement a]

data Pattern
    = PUnit
    | PAccess Name

data Exec a = Exec
    { _execRet :: Pattern
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

data Scope v f a = Scope
    { _scopeVars :: v
    , _scopeElem :: f a
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

data FuncSig = FuncSig
    { funcSigType :: Type
    , funcSigParamTypes :: [ByType]
    } deriving (Eq)

funcSig :: Func a -> FuncSig
funcSig func = FuncSig (func ^. funcType) (func ^. funcScope . scopeVars & map snd)
