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
    } deriving (Eq, Show)

data FuncSig
    = FuncSig
    { _funcName :: Name
    , _funcParams  :: Params
    , _funcRetType :: Type
    , _funcRetRefs :: [Type]
    } deriving (Eq, Show)

type Params
    = [(Name, Type)]

data Func
    = Func
    { _funcSig :: FuncSig
    , _funcStatement :: Statement
    } deriving (Eq, Show)

data Body
    = Body
    { _bodyVars  :: Vars
    , _bodyBinds :: [Bind]
    , _bodyResult :: Expression
    } deriving (Eq, Show)

data Bind
    = Bind
    { _bindPattern :: Pattern
    , _bindStatement :: Statement
    } deriving (Eq, Show)

data Statement
    = Assign Expression
    | Execute Name [Expression]
    | ForStatement ForCycle
    | MultiIfStatement (MultiIf Statement)
    | BodyStatement Body
    deriving (Eq, Show)

data ForCycle
    = ForCycle
    { _forLambda  :: Lambda
    , _forArgExpr :: Expression
    , _forRange   :: Expression
    } deriving (Eq, Show)

data Lambda
    = Lambda
    { _lamPatterns :: [Pattern]
    , _lamAction :: Statement
    } deriving (Eq, Show)

data MultiIf a
    = MultiIf
    { _multiIfLeafs :: [(Expression, a)]
    } deriving (Eq, Show)

data Expression
    = Access Name Index
    | Fold Name Expression Expression
    | Call Name [Expression]
    | Primary Literal
    | Tuple [Expression]
    | MultiIfExpression (MultiIf Expression)
    deriving (Eq, Show)

data Index
    = Index Integer
    | Immutable
    | Uninitialized
    deriving (Eq, Ord, Show)

type Indices
    = M.Map Name Index

data Pattern
    = PTuple [Pattern]
    | PAccess Name Index
    | PWildCard
    deriving (Eq, Show)

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
