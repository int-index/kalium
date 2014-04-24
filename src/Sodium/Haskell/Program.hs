module Sodium.Haskell.Program where

type Name = String

data HsType
	= HsUnit
	| HsIO HsType
	| HsType Name
	deriving (Show)

data Program
	= Program [Def] [String] [String]
	deriving (Show)

data Def = Def ValueDef
	deriving (Show)

data ValueDef
	= ValueDef Pattern Expression
	| GuardDef Pattern [(Expression, Expression)]
	deriving (Show)

data Expression
	= Primary Literal
	| Access Name
	| Lambda [Pattern] Expression
	| Beta Expression Expression
	| Tuple [Expression]
	| Typed Expression HsType
	| DoExpression [DoStatement]
	| PureLet [ValueDef] Expression
	| IfExpression Expression Expression Expression
	deriving (Show)

data Literal
	= Quote String
	| INumber String
	| FNumber String String
	| ENumber String String Bool String
	deriving (Show)

data DoStatement
	= DoBind Pattern Expression
	| DoLet  Pattern Expression
	| DoExecute Expression
	deriving (Show)

data Pattern
	= PatTuple [Name]
	| PatFunc Name [Name]
	deriving (Show)
