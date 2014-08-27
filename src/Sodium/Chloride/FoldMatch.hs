module Sodium.Chloride.FoldMatch (foldMatch) where

import Sodium.Chloride.Program.Vector
import Sodium.Chloride.Recmap.Vector
import Control.Lens (over)
import qualified Data.Map as M

foldMatch :: Program -> Program
foldMatch = over recmapped foldMatchStatement

foldMatchStatement :: Statement -> Statement
foldMatchStatement statement@(ForStatement forCycle)
    = maybe statement Assign (forCycleMatch forCycle)
foldMatchStatement statement = statement

forCycleMatch
    (ForCycle [(name1, j)] argExprs name2 range (Body vars [] [Call op args]))
    | M.null vars && args == [Access name1 j, Access name2 Immutable]
    = Just (foldMatchOn op argExprs range)
forCycleMatch _ = Nothing

foldMatchOn OpMultiply [Primary (INumber "1")] range
    = Call OpProduct [range]
foldMatchOn OpAdd [Primary (INumber "1")] range
    = Call OpSum [range]
foldMatchOn OpAnd [Primary BTrue] range
    = Call OpAnd' [range]
foldMatchOn OpOr [Primary BFalse] range
    = Call OpOr' [range]
foldMatchOn op argExprs range
    = Fold op argExprs range
