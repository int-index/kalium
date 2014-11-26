{-# LANGUAGE TemplateHaskell #-}
module Sodium.Nucleus.Program.Vector
    ( module Sodium.Nucleus.Program.Vector
    , module Sodium.Nucleus.Program
    ) where

import Control.Lens.TH
import qualified Data.Map as M

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
    , _funcLambda :: Lambda Statement
    } deriving (Eq)

data Body
    = Body
    { _bodyVars  :: M.Map (Name1 IndexTag) Type
    , _bodyBinds :: [Bind Statement]
    , _bodyResult :: Expression
    } deriving (Eq)

data Bind a
    = Bind
    { _bindPattern :: Pattern
    , _bindStatement :: a
    } deriving (Eq)

data Statement
    = Assign Expression
    | Execute (Name1 IndexTag) [Expression]
    | ForStatement (ForCycle Statement)
    | MultiIfStatement (MultiIf Statement)
    | BodyStatement Body
    deriving (Eq)

data ForCycle a
    = ForCycle
    { _forLambda  :: Lambda a
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
    | Fold (Name1 IndexTag) Expression Expression
    | Call (Name1 IndexTag) [Expression]
    | Primary Literal
    | MultiIfExpression (MultiIf Expression)
    deriving (Eq)

pattern CallOp2 op x y = Call (NameOp op) [x, y]

data IndexTag
    = IndexTag Integer
    | ImmutableTag
    | GlobalTag
    deriving (Eq, Ord, Show)

indexTag :: IndexTag -> Name1 () -> Name1 IndexTag
indexTag GlobalTag (NameOp op) = NameOp op
indexTag tag (Name1 ns _) = Name1 ns tag

retag :: Name -> Name1 IndexTag
retag = indexTag GlobalTag

data Pattern
    = PTuple Pattern Pattern
    | PAccess (Name1 IndexTag)
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

makePrisms ''Expression
makePrisms ''Statement
makePrisms ''Pattern
