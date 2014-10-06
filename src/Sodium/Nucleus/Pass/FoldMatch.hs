module Sodium.Nucleus.Pass.FoldMatch (foldMatch) where

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Control.Lens (over)

foldMatch :: Program -> Program
foldMatch = over recmapped foldMatchStatement

foldMatchStatement :: Statement -> Statement
foldMatchStatement statement@(ForStatement forCycle)
    = maybe statement Assign (forCycleMatch forCycle)
foldMatchStatement statement = statement

forCycleMatch
    (ForCycle (PAccess name1 j) argExpr name2 range (Assign (Call op args)))
    | args == [Access name1 j, Access name2 Immutable]
    = Just (Fold op argExpr range)
forCycleMatch _ = Nothing
