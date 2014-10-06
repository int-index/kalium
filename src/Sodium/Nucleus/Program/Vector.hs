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

data Func
    = Func
    { _funcSig :: FuncSig
    , _funcBody :: Body
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
    | Execute Operator [Expression]
    | ForStatement ForCycle
    | MultiIfStatement MultiIfBranch
    | BodyStatement Body
    deriving (Eq, Show)

data ForCycle
    = ForCycle
    { _forArgPattern :: Pattern
    , _forArgExpr    :: Expression
    , _forName   :: Name
    , _forRange  :: Expression
    , _forAction :: Statement
    } deriving (Eq, Show)

data MultiIfBranch
    = MultiIfBranch
    { _multiIfLeafs :: [(Expression, Statement)]
    , _multiIfElse  :: Statement
    } deriving (Eq, Show)

data Expression
    = Access Name Index
    | Fold Operator Expression Expression
    | Call Operator [Expression]
    | Primary Literal
    | Tuple [Expression]
    deriving (Eq, Show)

data Index
    = Index Integer
    | Immutable
    | Uninitialized
    deriving (Eq, Show)

type Indices
    = M.Map Name Index

data Pattern
    = PTuple [Pattern]
    | PAccess Name Index
    | PWildCard
    deriving (Eq, Show)

makeLenses ''Func
makeLenses ''Bind
makeLenses ''Body
makeLenses ''ForCycle
makeLenses ''MultiIfBranch
makeLenses ''Program

makePrisms ''Statement
makePrisms ''Pattern
