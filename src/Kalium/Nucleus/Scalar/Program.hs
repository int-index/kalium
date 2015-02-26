{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

data Program param pat expr = Program
    { _programFuncs :: Map Name (Func param pat expr)
    }

data By
    = ByValue
    | ByReference
    deriving (Eq, Show)

type ByType = (By, Type)

data Func param pat expr = Func
    { _funcType :: Type
    , _funcScope :: Scope (Pairs Name param) (Scope Vars Body) pat expr
    }

data Body pat expr = Body
    { _bodyStatement :: Statement pat expr
    , _bodyResult :: expr
    }

data Statement pat expr
    = Execute (Exec pat expr)
    | ForStatement (ForCycle pat expr)
    | IfStatement (If pat expr)
    | forall vars . Scoping vars => ScopeStatement (Scope vars Statement pat expr)
    | Follow (Statement pat expr) (Statement pat expr)
    | Pass

data Pattern
    = PUnit
    | PWildCard
    | PAccess Name
    | PTuple Pattern Pattern

data Exec pat expr = Exec
    { _execRet :: pat
    , _execOp :: Name
    , _execTyArgs :: [Type]
    , _execArgs :: [expr]
    }

data ForCycle pat expr
    = ForCycle
    { _forName :: Name
    , _forRange :: expr
    , _forStatement :: Statement pat expr
    }

data If pat expr = If
    { _ifCond :: expr
    , _ifThen :: Statement pat expr
    , _ifElse :: Statement pat expr
    }

data Scope vars obj pat expr = Scope
    { _scopeVars :: vars
    , _scopeElem :: obj pat expr
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

funcSig :: Typing param => Func param a p -> FuncSig
funcSig func = FuncSig
    (func ^. funcType)
    (func ^. funcScope . scopeVars & map (\(_name, ty) -> typing ty))
