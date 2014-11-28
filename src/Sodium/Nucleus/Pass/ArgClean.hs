{-# LANGUAGE ViewPatterns #-}
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

getDeepCall :: Name1 IndexTag -> Expression -> Maybe [Expression]
getDeepCall name (Access name1) | name == name1 = Just []
getDeepCall name (Call expr1 expr2) = (:) expr2 `fmap` getDeepCall name expr1
getDeepCall _ _ = Nothing

argCleanFunc :: Func -> Writer (Endo Expression) Func
argCleanFunc (Func sig (LambdaStatement (Lambda pats statement))) = do
    let mparams = map tag pats
    let op = sig ^. funcName
        f expr | Just (reverse -> args) <- getDeepCall op expr
               = foldl1 Call (Access op : catMaybes (zipWith untag mparams args))
               | otherwise = expr
    tell (Endo f)
    let sig'  = sig & funcParamTypes %~ catMaybes . zipWith untag mparams
        pats' = catMaybes mparams
    return (Func sig' (LambdaStatement (Lambda pats' statement)))
    where tag param@(PAccess name) | checkRef statement name = Just param
          tag _ = Nothing
          untag Nothing  _ = Nothing
          untag (Just _) x = Just x
argCleanFunc func = return func
