{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Sanity where

import qualified Data.List as L
import Control.Lens
import Control.Applicative
import Control.Monad.Writer

import Sodium.Nucleus.Vector.Program

sanity_nameUniqueness :: Program -> Bool
sanity_nameUniqueness program
    = (\xs -> xs == L.nub xs) . execWriter
    -- TODO: take function names into account (itraverse?)
    $ (programFuncs . traversed . funcExpression) (\e -> e <$ snu e) program
            
snu :: MonadWriter [Name] m => Expression -> m ()
snu = \case
    Access  _ -> return ()
    Primary _ -> return ()
    Lambda p e1 -> do
        psnu p
        snu e1
    Beta e1 e2 -> do
        snu e1
        snu e2

psnu :: MonadWriter [Name] m => Pattern -> m ()
psnu = \case
    PTuple p1 p2 -> do
        psnu p1
        psnu p2
    PAccess name _ -> do
        tell [name]
    PWildCard -> return ()
    PUnit -> return ()
