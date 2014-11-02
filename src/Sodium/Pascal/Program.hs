module Sodium.Pascal.Program where

import qualified Data.Map as M

type Name = String

data Program
	= Program [Func] Vars Body
	deriving (Show)

type Vars = M.Map Name Type
type Params = [ParamDecl]

type Body = [Statement]

data Func
    = Func Name Params (Maybe Type) Vars Body
    deriving (Show)

data ParamDecl
    = ParamDecl Name (By, Type)
    deriving (Show)

data By
    = ByReference
    | ByValue
    deriving (Eq, Show)

data Statement
    = Assign Name Expression
    | Execute Name [Expression]
    | ForCycle Name Expression Expression Statement
    | IfBranch Expression Statement (Maybe Statement)
    | CaseBranch Expression [([Expression], Statement)] (Maybe Statement)
    | BodyStatement Body
    deriving (Show)

data Expression
    = Access Name
    | Call (Either Operator Name) [Expression]
    | Primary Literal
    deriving (Show)

data Literal
    = LitBool Bool
    | LitInt  Integer
    | LitReal Rational
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
    | OpEquals
    | OpAnd
    | OpOr
    | OpXor
    | OpRange
    | OpNegate
    | OpPlus
    | OpNot
    deriving (Show)

data Type
    = TypeInteger
    | TypeReal
    | TypeBoolean
    | TypeString
    | TypeArray Type
    | TypeChar
    | TypeCustom Name
    deriving (Eq, Show)
