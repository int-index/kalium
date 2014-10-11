module Sodium.Nucleus.Pass.FoldMatch (foldMatch) where

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Control.Lens (over)

import Sodium.Util (tryApply)

foldMatch :: Program -> Program
foldMatch = over recmapped (tryApply forCycleMatch)

forCycleMatch :: Statement -> Maybe Statement
forCycleMatch
    (ForStatement (ForCycle pat argExpr name2 range action))
    | PAccess name1 j <- pat
    , Assign (Call op args) <- action
    , args == [Access name1 j, Access name2 Immutable]
    = Just (Assign $ Fold op argExpr range)
forCycleMatch _ = Nothing
