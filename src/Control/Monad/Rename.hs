{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Rename
    ( MonadRename
    , mkname
    , rename
    , RenameT
    , runRenameT
    ) where

import Prelude
import qualified Data.Map as M

import Control.Ether.TH (ethereal)
import Control.Monad.Ether.State

ethereal "Rename" "rnm"

type InternalState i s = (M.Map i s, i)

type MonadRename i s = MonadState Rename (InternalState i s)
type RenameT i s = StateT Rename (InternalState i s)

mkname :: (Ord i, Enum i, MonadRename i s m) => Maybe s -> m i
mkname ms = do
    (table, seed) <- get rnm
    let tabmod = case ms of
          Nothing -> id
          Just s  -> M.insert seed s
    put rnm (tabmod table, succ seed)
    return seed

rename :: (Ord i, Enum i, MonadRename i s m) => i -> m i
rename i = gets rnm (M.lookup i . fst) >>= mkname

runRenameT :: Monad m => RenameT i s m a -> i -> m (a, M.Map i s)
runRenameT t i = do
    (a, (table, _)) <- runStateT rnm t (M.empty, i)
    return (a, table)
