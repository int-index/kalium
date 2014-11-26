
module Sodium.Nucleus.Pass.ArgClean (argClean) where

import Data.Monoid
import Data.Maybe
import Control.Monad.Writer
import Control.Lens

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Name

argClean :: Program -> Program
argClean program = p & over recmapped (appEndo f)
    where (p, f) = runWriter (program & (programFuncs . traversed) argCleanFunc)

argCleanFunc :: Func -> Writer (Endo Expression) Func
argCleanFunc func = do
    let mparams = map tag $ func ^. funcLambda . lamPatterns
    let f (Call op args) | op == func ^. funcSig . funcName
            = Call op (catMaybes $ zipWith untag mparams args)
        f expr = expr
    tell (Endo f)
    func & funcLambda . lamPatterns .~ catMaybes mparams
         & funcSig . funcParamTypes %~ catMaybes . zipWith untag mparams
         & return
    where tag param@(PAccess name)
            | checkRef (func ^. funcLambda . lamAction) name = Just param
          tag _ = Nothing
          untag Nothing  _ = Nothing
          untag (Just _) x = Just x
