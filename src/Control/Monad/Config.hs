{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Config
    ( MonadConfig(..)
    , ConfigT(..)
    , runConfigT
    ) where

import Prelude
import Data.Coerce

import Control.Monad.Signatures
import qualified Control.Monad.Trans.Reader as Reader
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

class Monad m => MonadConfig conf m | m -> conf where
    config :: m conf

newtype ConfigT conf m a = ConfigT (ReaderT conf m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

instance Monad m => MonadConfig conf (ConfigT conf m) where
    config = ConfigT ask

instance MonadConfig conf m => MonadConfig conf (ExceptT e m) where
    config = lift config

instance MonadConfig conf m => MonadConfig conf (StateT s m) where
    config = lift config

instance MonadConfig conf m => MonadConfig conf (ReaderT r m) where
    config = lift config

instance (Monoid w, MonadConfig conf m) => MonadConfig conf (WriterT w m) where
    config = lift config

liftCatch :: Catch e m a -> Catch e (ConfigT conf m) a
liftCatch f m h = ConfigT $ Reader.liftCatch f (coerce m) (coerce h)

instance MonadError e m => MonadError e (ConfigT conf m) where
    throwError = lift . throwError
    catchError = liftCatch catchError

instance MonadState s m => MonadState s (ConfigT conf m) where
    get = lift get
    put = lift . put

runConfigT :: ConfigT conf m a -> conf -> m a
runConfigT = runReaderT . coerce

