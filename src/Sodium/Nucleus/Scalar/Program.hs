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

class Scoping v where
    scoping :: v -> M.Map Name Type

instance Typing t => Scoping (M.Map Name t) where
    scoping = M.map typing
instance Typing t => Scoping (Pairs Name t) where
    scoping = scoping . M.fromList

data Program a p = Program
    { _programFuncs :: M.Map Name (Func a p)
    }

data By
    = ByValue
    | ByReference
    deriving (Eq, Show)

type ByType = (By, Type)

type Params = [(Name, ByType)]

data Func a p = Func
    { _funcType :: Type
    , _funcScope :: Scope Params (Scope Vars Body) a p
    }

data Body a p = Body
    { _bodyStatement :: Statement a p
    , _bodyResult :: Atom
    }

data Statement a p
    = Execute (Exec a p)
    | ForStatement (ForCycle a p)
    | IfStatement (If a p)
    | forall v . Scoping v => ScopeStatement (Scope v Statement a p)
    | Group [Statement a p]

data Pattern
    = PUnit
    | PAccess Name

data Exec a p = Exec
    { _execRet :: p
    , _execOp :: Name
    , _execArgs :: [a]
    }

data ForCycle a p
    = ForCycle
    { _forName :: Name
    , _forRange :: a
    , _forStatement :: Statement a p
    }

data If a p = If
    { _ifCond :: a
    , _ifThen :: Statement a p
    , _ifElse :: Statement a p
    }

data Scope v f a p = Scope
    { _scopeVars :: v
    , _scopeElem :: f a p
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

funcSig :: Func a p -> FuncSig
funcSig func = FuncSig (func ^. funcType) (func ^. funcScope . scopeVars & map snd)
