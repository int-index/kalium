{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
module Sodium.Nucleus.Program where

import Data.Type.Equality
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

data Type
    = TypeInteger
    | TypeDouble
    | TypeBoolean
    | TypeString
    | TypeUnit
    | TypeList Type
    deriving (Eq, Ord, Show)

data SType :: Type -> * where
    STypeInteger :: SType TypeInteger
    STypeDouble  :: SType TypeDouble
    STypeBoolean :: SType TypeBoolean
    STypeString  :: SType TypeString
    STypeUnit    :: SType TypeUnit
    STypeList    :: SType t -> SType (TypeList t)

stype_to_type :: SType t -> Type
stype_to_type = \case
    STypeInteger -> TypeInteger
    STypeDouble  -> TypeDouble
    STypeBoolean -> TypeBoolean
    STypeString  -> TypeString
    STypeUnit    -> TypeUnit
    STypeList t1 -> TypeList (stype_to_type t1)

deriving instance Eq   (SType t)
deriving instance Show (SType t)

data DLiteral where
    DLit :: (r ~ TypeRepr t, Eq r, Show r) => SType (t :: Type) -> r -> DLiteral

deriving instance Show DLiteral

instance TestEquality SType where
    testEquality STypeInteger STypeInteger = Just Refl
    testEquality STypeDouble  STypeDouble  = Just Refl
    testEquality STypeBoolean STypeBoolean = Just Refl
    testEquality STypeString  STypeString  = Just Refl
    testEquality STypeUnit    STypeUnit    = Just Refl
    testEquality (STypeList t1) (STypeList t2)
        = case testEquality t1 t2 of
            Just Refl -> Just Refl
            Nothing   -> Nothing
    testEquality _ _ = Nothing

instance Eq DLiteral where
    DLit t1 r1 == DLit t2 r2 = case testEquality t1 t2 of
        Just Refl -> r1 == r2
        Nothing -> False

typecheckDLiteral :: DLiteral -> Type
typecheckDLiteral (DLit t _) = stype_to_type t

type family TypeRepr (t :: Type) where
    TypeRepr TypeInteger = Integer
    TypeRepr TypeDouble  = Rational
    TypeRepr TypeBoolean = Bool
    TypeRepr TypeString  = String
    TypeRepr TypeUnit    = ()
    TypeRepr (TypeList ts) = [TypeRepr ts]

type Vars
    = M.Map Name Type
