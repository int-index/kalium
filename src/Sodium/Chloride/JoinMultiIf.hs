module Sodium.Chloride.JoinMultiIf (joinMultiIf) where

import Control.Lens
import Sodium.Chloride.Program.Scalar
import Sodium.Chloride.Recmap.Scalar

joinMultiIf :: Program -> Program
joinMultiIf = recmapProgram' (recmapper' joinMultiIfStatement)

joinMultiIfStatement
	= _MultiIfStatement %~ tryApply joinMultiIfBranch
	where joinMultiIfBranch multiIfBranch
		 =  multiIfBranch ^? multiIfElse . bodySingleton . _MultiIfStatement
		<&> over multiIfLeafs (view multiIfLeafs multiIfBranch ++)

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)
