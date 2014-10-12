module Sodium.Nucleus.Pass.Clean (clean) where

import Control.Lens hiding (Index, Fold)
import Control.Monad.Writer
import qualified Data.Map as M
import Data.List
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Name

clean :: Program -> Program
clean = runIdentity . recmap cleaner

cleaner =  recmapper (return . cleanVars)
        <> recmapper (return . cleanBody)
        <> recmapper (return . cleanStatement)

cleanVars :: Body -> Body
cleanVars body = (bodyVars %~ M.filterWithKey cc) body where
    cc name _ = checkRef (bodyComponents body) name
    bodyComponents body = (body ^. bodyResult, body ^. bodyBinds)

cleanBody :: Body -> Body
cleanBody body = body & bodyBinds .~ binds where
    cleanBind [] = []
    cleanBind (bind:binds) = [bind & bindPattern %~ cleanUsage scope]
        where scope = (binds, body ^. bodyResult)
    binds = tails (body ^. bodyBinds) >>= cleanBind

cleanUsage :: Mask scope => scope -> Pattern -> Pattern
cleanUsage scope (PTuple ps)
  = case cleanUsage scope `map` ps of
      []  -> PWildCard
      [p] -> p
      ps' -> PTuple ps'
cleanUsage scope (PAccess name _)
  | not (checkRef scope name) = PWildCard
cleanUsage _ pat = pat

cleanStatement :: Statement -> Statement
cleanStatement = over _ForStatement cleanForCycle
  where
    cleanForCycle :: ForCycle -> ForCycle
    cleanForCycle forCycle
      = forCycle &  forLambda . lamPatterns . traversed
                 %~ cleanUsage (forCycle ^. forLambda . lamAction)
