module Sodium.Chloride.ExtractBody (extractBody) where

import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program.Vector
import Sodium.Chloride.Recmap.Vector

extractBody :: Program -> Program
extractBody = recmapProgram' (recmapper' extractBodyStatement)

extractBodyStatement :: Statement -> Statement
extractBodyStatement statement@(BodyStatement body)
	= maybe statement Assign (bodyMatch body)
extractBodyStatement statement = statement

bodyMatch body
	| M.null (body ^. bodyVars) && null (body ^. bodyBinds)
	= case body ^. bodyResults of
		[expr] -> Just expr
		_ -> Nothing
bodyMatch _ = Nothing
