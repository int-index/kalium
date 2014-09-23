module Sodium.Nucleus.Pass.JoinMultiIf (joinMultiIf) where

import Control.Lens
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

joinMultiIf :: Program -> Program
joinMultiIf = over recmapped joinMultiIfStatement

joinMultiIfStatement :: Statement -> Statement
joinMultiIfStatement
    = _MultiIfStatement %~ tryApply joinMultiIfBranch
    where joinMultiIfBranch multiIfBranch
            =  multiIfBranch ^? multiIfElse . _MultiIfStatement
           <&> over multiIfLeafs (view multiIfLeafs multiIfBranch ++)

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)
