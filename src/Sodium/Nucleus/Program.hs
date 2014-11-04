{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Sodium.Nucleus.Program where

import Data.Singletons.TH
import Data.Type.Equality
import qualified Data.Map as M

singletons [d|

    data Type
        = TypeInteger
        | TypeDouble
        | TypeBoolean
        | TypeChar
        | TypeUnit
        | TypeList Type
        deriving (Eq)

             |]

deriving instance Ord  Type
deriving instance Show Type

data Name
    = NameSpace String Name
    | NameMain
    | NameOp Operator
    | Name String
    deriving (Eq, Ord, Show)

data Operator
    = OpAdd
    | OpSubtract
    | OpMultiply
    | OpDivide
    | OpDiv
    | OpMod
    | OpLess
    | OpMore
    | OpEquals
    | OpAnd
    | OpOr
    | OpNot
    | OpXor
    | OpRange
    | OpElem
    | OpShow
    | OpNegate
    | OpProduct
    | OpSum
    | OpAnd'
    | OpOr'
    | OpPrintLn
    | OpGetLn
    | OpReadLn Type
    | OpId
    | OpFst
    | OpSnd
    | OpSingleton
    deriving (Eq, Ord, Show)

data Literal where
    Lit :: (r ~ TypeRepr t, Eq r, Show r) => SType t -> r -> Literal

deriving instance Show (Sing (t :: Type))
deriving instance Show Literal

instance Eq Literal where
    Lit t1 r1 == Lit t2 r2
        | Just Refl <- testEquality t1 t2
        = r1 == r2
    _ == _ = False

typecheckLiteral :: Literal -> Type
typecheckLiteral (Lit t _) = fromSing t

type family TypeRepr (t :: Type) where
    TypeRepr TypeInteger = Integer
    TypeRepr TypeDouble  = Rational
    TypeRepr TypeBoolean = Bool
    TypeRepr TypeChar    = Char
    TypeRepr TypeUnit    = ()
    TypeRepr (TypeList ts) = [TypeRepr ts]

type Vars
    = M.Map Name Type
