module Sodium.Nucleus.Pass.Clean (clean) where

import Control.Lens hiding (Index, Fold)
import Control.Monad.Writer
import Data.List
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Name

clean :: Program -> Program
clean = runIdentity . recmap cleaner

cleaner = recmapper (return . cleanBody) <> recmapper (return . cleanStatement)

cleanBody :: Body -> Body
cleanBody body = body & bodyBinds .~ binds where
    cleanBind [] = []
    cleanBind (bind:binds) = [bind & bindPattern %~ cleanUsage scope]
        where scope = (binds, body ^. bodyResult)
    binds = tails (body ^. bodyBinds) >>= cleanBind

cleanUsage :: Mask scope => scope -> Pattern -> Pattern
cleanUsage _ PUnit = PWildCard
cleanUsage scope (PTuple pat1 pat2) = PTuple
  (cleanUsage scope pat1)
  (cleanUsage scope pat2)
cleanUsage scope (PAccess name)
  | not (checkRef scope name) = PWildCard
cleanUsage _ pat = pat

cleanStatement :: Statement -> Statement
cleanStatement = over _ForStatement cleanForCycle
  where
    cleanForCycle :: ForCycle Statement -> ForCycle Statement
    cleanForCycle forCycle
      = forCycle &  forLambda . lamPatterns . traversed
                 %~ cleanUsage (forCycle ^. forLambda . lamAction)
