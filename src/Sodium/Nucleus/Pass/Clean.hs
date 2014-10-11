module Sodium.Nucleus.Pass.Clean (clean) where

import Control.Applicative
import Control.Lens hiding (Index, Fold)
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Monoid
import Data.List
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

clean :: Program -> Program
clean = runIdentity . recmap cleaner

cleaner =  recmapper (return . cleanVars)
        <> recmapper (return . cleanBody)
        <> recmapper (return . cleanStatement)

cleanVars :: Body -> Body
cleanVars body = (bodyVars %~ M.filterWithKey cc) body where
    cc name _ = runReader (checkRef $ bodyComponents body) name

cleanBody :: Body -> Body
cleanBody body = body & bodyBinds .~ binds where
    cleanBind [] = []
    cleanBind (bind:binds) = [bind & bindPattern %~ cleanUsage scope]
        where scope = (binds, body ^. bodyResult)
    binds = tails (body ^. bodyBinds) >>= cleanBind

cleanUsage :: CheckRef scope => scope -> Pattern -> Pattern
cleanUsage scope (PTuple ps)
  = case cleanUsage scope `map` ps of
      []  -> PWildCard
      [p] -> p
      ps' -> PTuple ps'
cleanUsage scope (PAccess name _)
  | not (checkRef scope `runReader` name) = PWildCard
cleanUsage _ pat = pat

cleanStatement :: Statement -> Statement
cleanStatement = over _ForStatement cleanForCycle
  where
    cleanForCycle :: ForCycle -> ForCycle
    cleanForCycle forCycle
      = forCycle & forArgPattern %~ cleanUsage (forCycle ^. forAction)

class CheckRef a where
    checkRef :: a -> Reader Name Bool


-- Helper instances

instance CheckRef a => CheckRef [a] where
    checkRef as = or <$> traversed checkRef as

instance (CheckRef a, CheckRef b) => CheckRef (a, b) where
    checkRef (a, b) = (||) <$> checkRef a <*> checkRef b


-- Actual instances

instance CheckRef Expression where
    checkRef = \case
        Primary _ -> return False
        Tuple exprs -> checkRef exprs
        Access name _ -> (==name) <$> ask
        Call _ exprs -> -- Check the operator?
            checkRef exprs
        Fold _ exprs range -> checkRef (exprs, range)
        MultiIfExpression multiIf -> checkRef multiIf

instance CheckRef Statement where
    checkRef = \case
        Execute _ exprs -> checkRef exprs
        Assign expr -> checkRef expr
        BodyStatement body -> checkRef body
        ForStatement forCycle -> checkRef forCycle
        MultiIfStatement multiIf -> checkRef multiIf

instance CheckRef Bind where
    checkRef = checkRef . view bindStatement

instance CheckRef ForCycle where
    checkRef forCycle = checkRef
        ((forCycle ^. forRange, forCycle ^. forArgExpr), forCycle ^. forAction)

instance CheckRef a => CheckRef (MultiIf a) where
    checkRef multiIf = checkRef (multiIf ^. multiIfLeafs)

bodyComponents body = (body ^. bodyResult, body ^. bodyBinds)

instance CheckRef Body where
    checkRef body = checkRef (bodyComponents body)
