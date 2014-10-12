module Sodium.Pascal.Program where

type Name = String

data Program
	= Program [Func] Vars Body
	deriving (Show)

type Vars = [VarDecl]
type Params = [ParamDecl]

type Body = [Statement]

data Func
    = Func Name Params (Maybe PasType) Vars Body
    deriving (Show)

data VarDecl
    = VarDecl [Name] PasType
    deriving (Show)

data ParamDecl
    = ParamDecl [Name] Bool PasType
    deriving (Show)

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
	| Call Name [Expression]
	| INumber String
	| FNumber String String
	| ENumber String String Bool String
	| Quote String
	| BTrue | BFalse
	| Binary Operator Expression Expression
	| Unary UnaryOperator Expression
	deriving (Show)

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
    | OpNot
    | OpRange
    deriving (Show)

data UnaryOperator
    = UOpNegate
    | UOpPlus
    | UOpNot
    deriving (Show)

data PasType
	= PasInteger
	| PasLongInt
	| PasReal
	| PasBoolean
	| PasString
	| PasArray PasType
	| PasType Name
	deriving (Eq, Show)
