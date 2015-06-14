{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Config
    ( MonadConfig
    , config
    , ConfigT
    , runConfigT
    ) where

import Control.Ether.TH (ethereal)
import Control.Monad.Ether.Reader

ethereal "Conf" "conf"

type MonadConfig = MonadReader Conf
type ConfigT = ReaderT Conf

config :: MonadConfig conf m => m conf
config = ask conf

runConfigT :: ConfigT conf m a -> conf -> m a
runConfigT = runReaderT conf
