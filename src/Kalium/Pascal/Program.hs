module Kalium.Pascal.Program where

import Kalium.Prelude

type Name = String

data Program
    = Program [Func] Vars Body
    deriving (Eq, Show)

type Vars = Map Name Type

type Body = [Statement]

data FuncSig = FuncSig [ParamDecl] (Maybe Type)
    deriving (Eq, Show)

data Func
    = Func Name FuncSig Vars Body
    deriving (Eq, Show)

data ParamDecl
    = ParamDecl Name (By, Type)
    deriving (Eq, Show)

data By
    = ByReference
    | ByValue
    deriving (Eq, Show)

type Leaf = ([Either Expression (Expression, Expression)], Statement)

data Statement
    = Assign Name (Maybe Expression) Expression
    | Execute Name [Expression]
    | ForCycle Name Expression Expression Statement
    | IfBranch Expression Statement (Maybe Statement)
    | CaseBranch Expression [Leaf] (Maybe Statement)
    | BodyStatement Body
    deriving (Eq, Show)

data Expression
    = Access Name
    | Call (Either Operator Name) [Expression]
    | Primary Literal
    deriving (Eq, Show)

data Literal
    = LitBool Bool
    | LitInt  Integer
    | LitReal Rational
    | LitChar Char
    | LitStr  String
    deriving (Eq, Show)

data Operator
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
    | OpXor
    | OpNegate
    | OpPlus
    | OpNot
    | OpIx
    | OpCharToString
    | OpIntToReal
    deriving (Eq, Show)

data Type
    = TypeInteger
    | TypeReal
    | TypeBoolean
    | TypeString
    | TypeArray Type
    | TypeChar
    | TypeCustom Name
    deriving (Eq, Show)
