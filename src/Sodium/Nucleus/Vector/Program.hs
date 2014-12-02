{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Vector.Program
    ( module Sodium.Nucleus.Vector.Program
    , module Sodium.Nucleus.Program
    ) where

import Control.Lens.TH

import Sodium.Nucleus.Program

data Program
    = Program
    { _programFuncs :: [Func]
    } deriving (Eq)

data Func
    = Func
    { _funcType :: Type
    , _funcName :: Name1 IndexTag
    , _funcExpression :: Expression
    } deriving (Eq)

follow pat statement result = bind statement (Lambda pat result)
bind a1 a2 = OpAccess OpBind `App` a1 `App` a2

data Expression
    = Atom Atom
    | Lambda Pattern Expression
    | App Expression Expression
    deriving (Eq)

pattern App2 op a1 a2 = op `App` a1 `App` a2

taint = App (OpAccess OpTaint)

lambda [] a = a
lambda (p:ps) a = Lambda p (lambda ps a)

data Atom
    = Access (Name1 IndexTag)
    | Primary Literal
    deriving (Eq)

pattern OpAccess op = Atom (Access (NameOp op))

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

makeLenses ''Func
makeLenses ''Program
