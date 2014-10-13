
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
    let mparams = map tag (func ^. funcSig . funcParams)
    let f (Call op args) | op == func ^. funcSig . funcName
            = Call op (catMaybes $ zipWith untag mparams args)
        f expr = expr
    tell (Endo f)
    return (func & funcSig . funcParams .~ catMaybes mparams)
    where tag param@(name, _)
            | checkRef (func ^. funcStatement) name = Just param
            | otherwise = Nothing
          untag Nothing  _ = Nothing
          untag (Just _) x = Just x
