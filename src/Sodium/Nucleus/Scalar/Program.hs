{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module Sodium.Nucleus.Scalar.Program
    ( module Sodium.Nucleus.Scalar.Program
    , module Sodium.Nucleus.Program
    , pos, peeks, store
    ) where

import Control.Lens
import Control.Comonad.Store
import qualified Data.Map as M

import Sodium.Nucleus.Program

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
    | forall v . ScopeStatement (Scope v Statement a)
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

data Scope v f a = Scope
    { _scopeVars :: Store v (M.Map Name Type)
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
funcSig func = FuncSig (func ^. funcType) (func ^. funcScope . scopeVars & pos & map snd)
