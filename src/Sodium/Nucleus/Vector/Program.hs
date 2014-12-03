{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Vector.Program
    ( module Sodium.Nucleus.Vector.Program
    , module Sodium.Nucleus.Program
    ) where

import Control.Lens.TH

import Sodium.Nucleus.Program

type Name = Name1 IndexTag

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

follow pat statement result = bind statement (Lambda pat result)
bind a1 a2 = OpAccess OpBind `App` a1 `App` a2

data Expression
    = Atom Atom
    | Lambda Pattern Expression
    | App Expression Expression
    deriving (Eq)

pattern App2 op a1 a2 = op `App` a1 `App` a2
pattern Literal ty repr = Atom (Primary (Lit ty repr))

taint = App (OpAccess OpTaint)

lambda [] a = a
lambda (p:ps) a = Lambda p (lambda ps a)

data Atom
    = Access Name
    | Primary Literal
    deriving (Eq)

pattern OpAccess op = Atom (Access (NameOp op))

data IndexTag
    = IndexTag Integer
    | ImmutableTag
    | GlobalTag
    deriving (Eq, Ord, Show)

data Pattern
    = PTuple Pattern Pattern
    | PAccess Name Type
    | PWildCard
    | PUnit
    deriving (Eq)

makeLenses ''Func
makeLenses ''Program
