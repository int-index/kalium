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
