module Sodium.Nucleus.Pass.ExtractBody (extractBody) where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Pattern

extractBody :: Program -> Program
extractBody = over recmapped extractBodyStatement

extractBodyStatement :: Statement -> Statement
extractBodyStatement statement@(BodyStatement body)
    = maybe statement id (bodyMatch body)
extractBodyStatement statement = statement

bodyMatch body
    | M.null (body ^. bodyVars) && null (body ^. bodyBinds)
        = return $ Assign (body ^. bodyResult)
    | otherwise = do
        -- TODO: propagate the bound variables
        -- onto the enclosing body
        guard $ M.null (body ^. bodyVars)
        [bind] <- return (body ^. bodyBinds)
        guard $ patMatch (bind ^. bindPattern) (body ^. bodyResult)
        return (bind ^. bindStatement)
