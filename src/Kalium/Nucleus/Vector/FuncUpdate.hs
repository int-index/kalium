{-# LANGUAGE FlexibleContexts #-}
module Kalium.Nucleus.Vector.FuncUpdate where

import Kalium.Prelude
import Kalium.Util

import Control.Monad.Trans.Maybe
import qualified Data.Map as M

import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Name (mentions)

type FuncUpdate m = Name -> Name -> Func -> MaybeT m (Endo' Func, Func)

funcUpdate :: MonadNameGen m => FuncUpdate m -> Program -> m Program
funcUpdate fnUpd program = do
    updates
        <-  catMaybes . M.elems
        <$> itraverse (reifyFuncUpdate fnUpd) (program ^. programFuncs)
    return $ (maybe program id . asum) (map ($program) updates)

reifyFuncUpdate
    :: MonadNameGen m => FuncUpdate m -> Name -> Func
    -> m (Maybe (Program -> Maybe Program))
reifyFuncUpdate fnUpd name func = runMaybeT $ do
    name' <- alias name
    (upd, func') <- fnUpd name name' func
    let
      substitute program
          | program' `mentions` name = Nothing
          | otherwise = Just program'
        where
          program' = program
            & programFuncs %~ M.insert name' func' . M.delete name
            & programFuncs . traversed %~ upd
    return substitute
