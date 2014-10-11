module Sodium.Nucleus.Pass.ExtractBody (extractBody) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Pattern
import Sodium.Util (tryApply)

extractBody :: Program -> Program
extractBody = over recmapped (tryApply bodyMatch)
            . over recmapped propagate

bodyMatch :: Statement -> Maybe Statement
bodyMatch (BodyStatement body)
    | M.null (body ^. bodyVars)
    ,   null (body ^. bodyBinds)
        = return $ Assign (body ^. bodyResult)
    | otherwise = do
        [bind] <- return (body ^. bodyBinds)
        guard $ expMatch (bind ^. bindPattern) (body ^. bodyResult)
        let vars = body ^. bodyVars
        if M.null vars
            then return (bind ^. bindStatement)
            else do
                body' <- bind ^? bindStatement . _BodyStatement
                guard $ M.null (body' ^. bodyVars)
                return (BodyStatement body')


bodyMatch _ = Nothing

bodyChildren :: Traversal' Body Body
bodyChildren = bodyBinds . traversed . bindStatement . _BodyStatement

propagate :: Body -> Body
propagate = execState $ do
    pvars <- use (bodyVars . to M.toList)
    cvars <- use (bodyChildren . bodyVars . to M.toList)
    let vars = pvars ++ cvars
    let uniq ty1 ty2 = do
          a <- ty1
          b <- ty2
          guard (a == b)
          return a
    let uniqVars = M.mapMaybe id
                 $ M.fromListWith uniq (vars & traversed . _2 %~ Just)
    bodyVars %= M.union uniqVars
    bodyChildren . bodyVars %= flip M.difference uniqVars
