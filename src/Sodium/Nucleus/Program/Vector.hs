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
    { _funcName :: Name
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
    { _bodyVars  :: Vars
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
    | Execute Name [Expression]
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
    = Access Name IndexTag
    | Fold Name Expression Expression
    | Call Name [Expression]
    | Primary Literal
    | MultiIfExpression (MultiIf Expression)
    deriving (Eq)

pattern CallOp2 op x y = Call (NameOp op) [x, y]

data IndexTag
    = IndexTag Integer
    | ImmutableTag
    deriving (Eq, Ord, Show)

data Pattern
    = PTuple Pattern Pattern
    | PAccess Name IndexTag
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
