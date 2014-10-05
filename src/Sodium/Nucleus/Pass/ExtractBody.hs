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
    = case body ^. bodyResults of
        [expr] -> return (Assign expr)
        _ -> mzero
        -- TODO: just wrap bodyResults in a tuple
        --       to allow multiple expressions

    | otherwise = do
        [bind] <- return (body ^. bodyBinds)
        let smartTuple [x] = x
            smartTuple xs  = Tuple xs
        guard $ patMatch (bind ^. bindPattern) (body ^. bodyResults . to smartTuple)
        return (bind ^. bindStatement)
