module Sodium.Nucleus.Pass.FoldMatch (foldMatch) where

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Control.Lens (over)

import Sodium.Util (tryApply)

foldMatch :: Program -> Program
foldMatch = over recmapped (tryApply forCycleMatch)

forCycleMatch :: Statement -> Maybe Statement
forCycleMatch
    (ForStatement (ForCycle (LambdaStatement lam) argExpr range))
    | Lambda [PAccess name1, PAccess name2] action <- lam
    , Assign (Call2 op arg1 arg2) <- action
    , arg1 == Access name1
    , arg2 == Access name2
    = Just (Assign $ Call3 (OpAccess OpFold) op argExpr range)
forCycleMatch _ = Nothing
