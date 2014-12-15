{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Vector.Program where

import Control.Lens.TH

data NameSpecial
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
    | OpTrue
    | OpFalse
    | OpRange
    | OpElem
    | OpShow
    | OpNegate
    | OpIf
    | OpFold
    | OpFoldTainted
    | OpProduct
    | OpSum
    | OpAnd'
    | OpOr'
    | OpPrintLn
    | OpReadLn
    | OpPutLn
    | OpGetLn
    | OpId
    | OpUnit
    | OpPair
    | OpFst
    | OpSnd
    | OpSwap
    | OpNil
    | OpCons
    | OpSingleton
    | OpTaint
    | OpBind
    | OpBindIgnore
    | OpFmapIgnore
    | OpIgnore
    | OpConcat
    | OpIntToDouble
    | OpUndefined
    | OpMain
    deriving (Eq, Ord, Show)

data Name = NameSpecial NameSpecial
          | NameGen Integer
    deriving (Eq, Ord)

data Type
    = TypeInteger
    | TypeDouble
    | TypeBoolean
    | TypeChar
    | TypeUnit
    | TypeList Type
    | TypePair Type Type
    | TypeFunction Type Type
    | TypeTaint Type
    deriving (Eq, Ord, Show)

data Literal
    = LitInteger Integer
    | LitDouble  Rational
    | LitChar    Char
    deriving (Eq, Show)

data Program
    = Program
    { _programFuncs :: [Func]
    } deriving (Eq)

data Func
    = Func
    { _funcType :: Type
    , _funcName :: Name
    , _funcExpression :: Expression
    } deriving (Eq)

data Expression
    = Access Name
    | Primary Literal
    | Lambda Pattern Expression
    | Beta Expression Expression
    deriving (Eq)

pattern OpAccess op = Access (NameSpecial op)
pattern LitUnit = OpAccess OpUnit

pattern App1 op a        = op `Beta` a
pattern App2 op a1 a2    = op `Beta` a1 `Beta` a2
pattern App3 op a1 a2 a3 = op `Beta` a1 `Beta` a2 `Beta` a3

pattern AppOp1 op a        = App1 (OpAccess op) a
pattern AppOp2 op a1 a2    = App2 (OpAccess op) a1 a2
pattern AppOp3 op a1 a2 a3 = App3 (OpAccess op) a1 a2 a3

pattern Lambda2 p1 p2 a = Lambda p1 (Lambda p2 a)

pattern Into p x a = Beta (Lambda p a) x
pattern Eta p x a = Lambda p (Beta x a)

pattern Ignore a     = AppOp1 OpIgnore a
pattern Taint  a     = AppOp1 OpTaint  a
pattern Bind   a1 a2 = AppOp2 OpBind   a1 a2
pattern Follow p x a = Bind x (Lambda p a)

lambda = flip (foldr Lambda)

unlambda :: Expression -> ([Pattern], Expression)
unlambda = \case
    Lambda p a -> let (ps, b) = unlambda a in (p:ps, b)
    e -> ([], e)

unbeta :: Expression -> [Expression]
unbeta = reverse . go where
    go = \case
        Beta a b -> b : go a
        e -> [e]

data Pattern
    = PTuple Pattern Pattern
    | PAccess Name Type
    | PWildCard
    | PUnit
    deriving (Eq)

makeLenses ''Func
makeLenses ''Program
