{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program.Vector
    ( module Sodium.Nucleus.Program.Vector
    , module Sodium.Nucleus.Program
    ) where

import Control.Lens.TH

import Sodium.Nucleus.Program

data Program
    = Program
    { _programFuncs :: [Func]
    } deriving (Eq)

data FuncSig
    = FuncSig
    { _funcName :: Name1 IndexTag
    , _funcParamTypes :: [Type]
    , _funcRetType :: Type
    } deriving (Eq)

data Func
    = Func
    { _funcSig :: FuncSig
    , _funcStatement :: Statement
    } deriving (Eq)

data Body a
    = Body
    { _bodyBinds :: [Bind a]
    , _bodyResult :: a
    } deriving (Eq)

data Bind a
    = Bind
    { _bindPattern :: Pattern
    , _bindStatement :: a
    } deriving (Eq)

data Statement
    = Assign Expression
    | Execute Expression
    | ForStatement ForCycle
    | MultiIfStatement (MultiIf Statement)
    | BodyStatement (Body Statement)
    | LambdaStatement (Lambda Statement)
    deriving (Eq)

data ForCycle
    = ForCycle
    { _forStatement :: Statement
    , _forArgExpr :: Expression
    , _forRange   :: Expression
    } deriving (Eq)

data Lambda a
    = Lambda
    { _lamPatterns :: [Pattern]
    , _lamAction :: a
    } deriving (Eq)

data MultiIf a
    = MultiIf
    { _multiIfLeafs :: [(Expression, a)]
    } deriving (Eq)

data Expression
    = Access (Name1 IndexTag)
    | Call Expression Expression
    | Primary Literal
    | MultiIfExpression (MultiIf Expression)
    | BodyExpression (Body Expression)
    deriving (Eq)

pattern OpAccess op = Access (NameOp op)
pattern Call2 a b c   = Call (Call  a b)   c
pattern Call3 a b c d = Call (Call2 a b c) d

data IndexTag
    = IndexTag Integer
    | ImmutableTag
    | GlobalTag
    deriving (Eq, Ord, Show)

indexTag :: IndexTag -> Name1 () -> Name1 IndexTag
indexTag GlobalTag (NameOp op) = NameOp op
indexTag tag (Name1 ns _) = Name1 ns tag
indexTag _ _ = error "indexTag: impossible"

retag :: Name -> Name1 IndexTag
retag = indexTag GlobalTag

data Pattern
    = PTuple Pattern Pattern
    | PAccess (Name1 IndexTag) Type
    | PWildCard
    | PUnit
    deriving (Eq)

makeLenses ''FuncSig
makeLenses ''Func
makeLenses ''Bind
makeLenses ''Body
makeLenses ''Lambda
makeLenses ''ForCycle
makeLenses ''MultiIf
makeLenses ''Program
