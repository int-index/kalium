module Sodium.Nucleus.Pass.JoinMultiIf (joinMultiIf) where

import Control.Lens
import Sodium.Nucleus.Program.Scalar
import Sodium.Nucleus.Recmap.Scalar

joinMultiIf :: Program -> Program
joinMultiIf = over recmapped joinMultiIfStatement

joinMultiIfStatement
    = _MultiIfStatement %~ tryApply joinMultiIfBranch
    where joinMultiIfBranch multiIfBranch
            =  multiIfBranch ^? multiIfElse . bodySingleton . _MultiIfStatement
           <&> over multiIfLeafs (view multiIfLeafs multiIfBranch ++)

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)
