{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Kalium.Nucleus.Scalar.Program where

import Kalium.Prelude
import Kalium.Util

import qualified Data.Map as M

data NameSpecial
    = OpAdd
    | OpSubtract
    | OpMultiply
    | OpDivide
    | OpDiv
    | OpMod
    | OpLess
    | OpMore
    | OpLessEquals
    | OpMoreEquals
    | OpEquals
    | OpNotEquals
    | OpAnd
    | OpOr
    | OpNot
    | OpXor
    | OpTrue
    | OpFalse
    | OpRange
    | OpElem
    | OpShow
    | OpNegate
    | OpPrintLn
    | OpReadLn
    | OpPutLn
    | OpPut
    | OpChr
    | OpChrOrd
    | OpGetLn
    | OpGetChar
    | OpId
    | OpUnit
    | OpPair
    | OpNil
    | OpCons
    | OpSingleton
    | OpIx
    | OpIxSet
    | OpLength
    | OpSetLength
    | OpConcat
    | OpIntToDouble
    | OpMain
    | OpTypeInteger
    | OpTypeDouble
    | OpTypeBoolean
    | OpTypeChar
    | OpTypeUnit
    | OpTypeList
    | OpTypePair
    deriving (Eq, Ord, Show)

data Name = NameSpecial NameSpecial
          | NameGen Integer
    deriving (Eq, Ord, Show)

data Type
    = TypeAccess Name
    | TypeBeta Type Type
    deriving (Eq, Ord, Show)

pattern TypeApp1 t t1    = t `TypeBeta` t1
pattern TypeApp2 t t1 t2 = t `TypeBeta` t1 `TypeBeta` t2

pattern TypeString  = TypeApp1 TypeList TypeChar
pattern TypeInteger = TypeAccess (NameSpecial OpTypeInteger)
pattern TypeDouble  = TypeAccess (NameSpecial OpTypeDouble)
pattern TypeBoolean = TypeAccess (NameSpecial OpTypeBoolean)
pattern TypeChar = TypeAccess (NameSpecial OpTypeChar)
pattern TypeUnit = TypeAccess (NameSpecial OpTypeUnit)
pattern TypeList = TypeAccess (NameSpecial OpTypeList)
pattern TypePair = TypeAccess (NameSpecial OpTypePair)

data Literal
    = LitInteger Integer
    | LitDouble  Rational
    | LitChar    Char
    deriving (Eq, Show)

type Vars = Map Name Type

class Typing t where
    typing :: t -> Type

instance Typing Type   where typing = id
instance Typing ByType where typing = snd

class Scoping vars where
    scoping :: vars -> Map Name Type

instance Typing t => Scoping (Map Name t) where
    scoping = fmap typing
instance Typing t => Scoping (Pairs Name t) where
    scoping = scoping . M.fromList


-- Configuration

data Configuration param pat expr

type family GetParameter a where
    GetParameter (Configuration param pat expr) = param

type family GetPattern a where
    GetPattern (Configuration param pat expr) = pat

type family GetExpression a where
    GetExpression (Configuration param pat expr) = expr

type ComplexConfiguration = Configuration ByType Pattern Expression
type PrimitiveConfiguration = Configuration Type Pattern Atom

type Complex   a = a ComplexConfiguration
type Primitive a = a PrimitiveConfiguration

-- AST

data Program config = Program
    { _programFuncs :: Map Name (Func config)
    }

data By
    = ByValue
    | ByReference
    deriving (Eq, Show)

type ByType = (By, Type)

data Func config = Func
    { _funcType :: Type
    , _funcScope :: Scope
        (Pairs Name (GetParameter config))
        (Scope Vars Body) config
    }

data Body config = Body
    { _bodyStatement :: Statement config
    , _bodyResult :: GetExpression config
    }

data Statement config
    = Execute (Exec config)
    | ForStatement (ForCycle config)
    | IfStatement (If config)
    | forall vars . Scoping vars => ScopeStatement (Scope vars Statement config)
    | Follow (Statement config) (Statement config)
    | Pass

data Pattern
    = PUnit
    | PWildCard
    | PAccess Name
    | PTuple Pattern Pattern

data Exec config = Exec
    { _execRet :: GetPattern config
    , _execOp :: Name
    , _execTyArgs :: [Type]
    , _execArgs :: [GetExpression config]
    }

data ForCycle config
    = ForCycle
    { _forName :: Name
    , _forRange :: GetExpression config
    , _forStatement :: Statement config
    }

data If config = If
    { _ifCond :: GetExpression config
    , _ifThen :: Statement config
    , _ifElse :: Statement config
    }

data Scope vars obj config = Scope
    { _scopeVars :: vars
    , _scopeElem :: obj config
    }

data Expression
    = Atom Atom
    | Call Name [Type] [Expression]

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
    , funcSigParamTypes :: [Type]
    } deriving (Eq)

funcSig :: Typing (GetParameter config) => Func config -> FuncSig
funcSig func = FuncSig
    (func ^. funcType)
    (func ^. funcScope . scopeVars & map (\(_name, ty) -> typing ty))
