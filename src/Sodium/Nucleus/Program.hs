module Sodium.Nucleus.Program where

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

data Literal
    = LitInteger Integer
    | LitDouble  Rational
    | LitChar    Char
    deriving (Eq, Show)

typecheckLiteral :: Literal -> Type
typecheckLiteral = \case
    LitInteger _ -> TypeInteger
    LitDouble  _ -> TypeDouble
    LitChar    _ -> TypeChar
