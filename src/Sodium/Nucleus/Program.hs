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
        | TypeString
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
    | OpReadLn Type
    | OpId
    | OpFst
    | OpSnd
    deriving (Eq, Ord, Show)

data Literal where
    Lit :: (r ~ TypeRepr t, Eq r, Show r) => SType t -> r -> Literal

deriving instance Show (Sing (t :: Type))
deriving instance Show Literal

instance Eq Literal where
    Lit t1 r1 == Lit t2 r2 = case testEquality t1 t2 of
        Just Refl -> r1 == r2
        Nothing -> False

typecheckLiteral :: Literal -> Type
typecheckLiteral (Lit t _) = fromSing t

type family TypeRepr (t :: Type) where
    TypeRepr TypeInteger = Integer
    TypeRepr TypeDouble  = Rational
    TypeRepr TypeBoolean = Bool
    TypeRepr TypeString  = String
    TypeRepr TypeUnit    = ()
    TypeRepr (TypeList ts) = [TypeRepr ts]

type Vars
    = M.Map Name Type
