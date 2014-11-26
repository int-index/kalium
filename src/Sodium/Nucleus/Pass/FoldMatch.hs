module Sodium.Nucleus.Pass.FoldMatch (foldMatch) where

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Control.Lens (over)

import Sodium.Util (tryApply)

foldMatch :: Program -> Program
foldMatch = over recmapped (tryApply forCycleMatch)

forCycleMatch :: Statement -> Maybe Statement
forCycleMatch
    (ForStatement (ForCycle lam argExpr range))
    | Lambda [PAccess name1, PAccess name2] action <- lam
    , Assign (Call op args) <- action
    , args == [Access name1, Access name2]
    = Just (Assign $ Fold op argExpr range)
forCycleMatch _ = Nothing
