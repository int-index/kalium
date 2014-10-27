{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Sodium.Nucleus.Program where

import Control.Monad (msum)
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
    LitInteger :: Integer   -> Literal TypeInteger
    LitDouble  :: Rational  -> Literal TypeDouble
    LitBoolean :: Bool      -> Literal TypeBoolean
    LitString  :: String    -> Literal TypeString
    LitUnit ::  Literal TypeUnit
    LitList :: [Literal t] -> Literal (TypeList t)

deriving instance Eq   (Literal t)
deriving instance Show (Literal t)

data Literal' = forall t . Literal' (Literal t)

deriving instance Show Literal'

instance Eq Literal' where
    Literal' l1 == Literal' l2 = maybe False id $ msum
        [ do LitInteger x <- Just l1
             LitInteger y <- Just l2
             return (x == y)
        , do LitDouble x <- Just l1
             LitDouble y <- Just l2
             return (x == y)
        , do LitBoolean x <- Just l1
             LitBoolean y <- Just l2
             return (x == y)
        , do LitString x <- Just l1
             LitString y <- Just l2
             return (x == y)
        , do LitUnit <- Just l1
             LitUnit <- Just l2
             return True
        , do LitList xs <- Just l1
             LitList ys <- Just l2
             let x' = map Literal' xs
             let y' = map Literal' ys
             return (x' == y')
        ]

pattern LitInteger' a = Literal' (LitInteger a)
pattern LitDouble'  a = Literal' (LitDouble  a)
pattern LitBoolean' a = Literal' (LitBoolean a)
pattern LitString'  a = Literal' (LitString  a)
pattern LitUnit'      = Literal' (LitUnit)

data Type
    = TypeInteger
    | TypeDouble
    | TypeBoolean
    | TypeString
    | TypeUnit
    | TypeList Type
    deriving (Eq, Ord, Show)

type Vars
    = M.Map Name Type
