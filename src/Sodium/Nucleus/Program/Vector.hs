{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program.Vector
    ( module Sodium.Nucleus.Program.Vector
    , module Sodium.Nucleus.Program
    ) where

import Control.Lens.TH

import Sodium.Nucleus.Program
import Sodium.Util

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

follow pat statement result
    = BindStatement statement (LambdaStatement pat result)

data Statement
    = Execute Expression
    | ForStatement Statement Statement Statement
    | IfStatement Statement Statement Statement
    | LambdaStatement Pattern Statement
    | BindStatement Statement Statement
    deriving (Eq)

assign a = Execute (Call (OpAccess OpTaint) a)

lambda [] a = a
lambda (p:ps) a = LambdaStatement p (lambda ps a)

data Expression
    = Access (Name1 IndexTag)
    | Call Expression Expression
    | Primary Literal
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
makeLenses ''Program
