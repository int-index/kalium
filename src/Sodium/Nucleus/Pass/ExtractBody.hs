module Sodium.Nucleus.Pass.ExtractBody (extractBody) where

import Control.Lens
import Control.Monad
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Pattern
import Sodium.Util (tryApply)

extractBody :: Program -> Program
extractBody = over recmapped (tryApply bodyMatch)

bodyMatch :: Statement -> Maybe Statement
bodyMatch (BodyStatement body)
    | null (body ^. bodyBinds) = return (body ^. bodyResult)
    | otherwise = do
        [bind] <- return (body ^. bodyBinds)
        Assign expr <- return (body ^. bodyResult)
        guard $ expMatch (bind ^. bindPattern) expr
        return (bind ^. bindStatement)
bodyMatch _ = Nothing
