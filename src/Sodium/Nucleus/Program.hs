{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Sodium.Nucleus.Program where

import qualified Data.Map as M

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

data Literal (t :: Type) where
    LitInteger :: TypeRepr TypeInteger -> Literal TypeInteger
    LitDouble  :: TypeRepr TypeDouble  -> Literal TypeDouble
    LitBoolean :: TypeRepr TypeBoolean -> Literal TypeBoolean
    LitString  :: TypeRepr TypeString  -> Literal TypeString
    LitUnit    :: TypeRepr TypeUnit    -> Literal TypeUnit
    LitSuper :: (r ~ TypeRepr t, Eq r, Show r) => r -> Literal t

deriving instance Eq   (Literal t)
deriving instance Show (Literal t)

data Literal' = forall t . Literal' (Literal t)

deriving instance Show Literal'

pattern LitInteger' a = Literal' (LitInteger a)
pattern LitDouble'  a = Literal' (LitDouble  a)
pattern LitBoolean' a = Literal' (LitBoolean a)
pattern LitString'  a = Literal' (LitString  a)
pattern LitUnit'    a = Literal' (LitUnit a)

instance Eq Literal' where
    LitInteger' x == LitInteger' y = x == y
    LitDouble'  x == LitDouble'  y = x == y
    LitBoolean' x == LitBoolean' y = x == y
    LitString'  x == LitString'  y = x == y
    LitUnit'    x == LitUnit'    y = x == y
    _ == _ = False

data Type
    = TypeInteger
    | TypeDouble
    | TypeBoolean
    | TypeString
    | TypeUnit
    | TypeList Type
    deriving (Eq, Ord, Show)

type family TypeRepr (t :: Type) where
    TypeRepr TypeInteger = Integer
    TypeRepr TypeDouble  = Rational
    TypeRepr TypeBoolean = Bool
    TypeRepr TypeString  = String
    TypeRepr TypeUnit    = ()
    TypeRepr (TypeList ts) = [TypeRepr ts]

type Vars
    = M.Map Name Type
