{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Vector.Program
    ( module Sodium.Nucleus.Vector.Program
    , module Sodium.Nucleus.Program
    ) where

import Control.Lens.TH

import Sodium.Nucleus.Program

data Name = NameOp  Operator
          | NameGen Integer
    deriving (Eq, Ord, Show)

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

data Expression
    = Access Name
    | Primary Literal
    | Lambda Pattern Expression
    | Beta Expression Expression
    deriving (Eq)

pattern OpAccess op = Access (NameOp op)
pattern LitUnit = OpAccess OpUnit

pattern App1 op a        = op `Beta` a
pattern App2 op a1 a2    = op `Beta` a1 `Beta` a2
pattern App3 op a1 a2 a3 = op `Beta` a1 `Beta` a2 `Beta` a3

pattern AppOp1 op a        = App1 (OpAccess op) a
pattern AppOp2 op a1 a2    = App2 (OpAccess op) a1 a2
pattern AppOp3 op a1 a2 a3 = App3 (OpAccess op) a1 a2 a3

pattern Eta p x a = Lambda p (Beta x a)

pattern Ignore a     = AppOp1 OpIgnore a
pattern Taint  a     = AppOp1 OpTaint  a
pattern Bind   a1 a2 = AppOp2 OpBind   a1 a2
pattern Follow p x a = Bind x (Lambda p a)

lambda = flip (foldr Lambda)

data Pattern
    = PTuple Pattern Pattern
    | PAccess Name Type
    | PWildCard
    | PUnit
    deriving (Eq)

makeLenses ''Func
makeLenses ''Program
