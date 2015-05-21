{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Rename
    ( MonadRename(..)
    , RenameT(..)
    , runRenameT
    ) where

import Prelude
import Data.Coerce
import qualified Data.Map as M

import Control.Monad.Signatures
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

class Monad m => MonadRename i s m | m -> i, m -> s where
    mkname :: (Ord i, Enum i) => Maybe s -> m i
    rename :: (Ord i, Enum i) =>       i -> m i

type InternalState i s = (M.Map i s, i)

newtype RenameT i s m a = RenameT (StateT (InternalState i s) m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

instance Monad m => MonadRename i s (RenameT i s m) where
    mkname ms = RenameT $ do
        (table, seed) <- get
        let tabmod = case ms of
              Nothing -> id
              Just s  -> M.insert seed s
        put (tabmod table, succ seed)
        return seed
    rename i = RenameT (gets (M.lookup i . fst)) >>= mkname

instance MonadRename i s m => MonadRename i s (ExceptT e m) where
    mkname = lift . mkname
    rename = lift . rename

instance MonadRename i s m => MonadRename i s (StateT st m) where
    mkname = lift . mkname
    rename = lift . rename

instance MonadRename i s m => MonadRename i s (ReaderT r m) where
    mkname = lift . mkname
    rename = lift . rename

instance (Monoid w, MonadRename i s m) => MonadRename i s (WriterT w m) where
    mkname = lift . mkname
    rename = lift . rename

instance MonadRename i s m => MonadRename i s (MaybeT m) where
    mkname = lift . mkname
    rename = lift . rename

liftCatch :: Catch e m (a, InternalState i s) -> Catch e (RenameT i s m) a
liftCatch catchE m h = coerce $ State.liftCatch catchE (coerce m) (coerce h)

instance MonadError e m => MonadError e (RenameT i s m) where
    throwError = lift . throwError
    catchError = liftCatch catchError

runRenameT :: Monad m => RenameT i s m a -> i -> m (a, M.Map i s)
runRenameT (RenameT t) i = do
    (a, (table, _)) <- runStateT t (M.empty, i)
    return (a, table)
