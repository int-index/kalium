{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Sanity where

import Sodium.Prelude
import Sodium.Nucleus.Vector.Program

sanity_nameUniqueness :: Program -> Bool
sanity_nameUniqueness program
    = liftA2 (==) nub id . execWriter
    $ itraverse fnsnu (view programFuncs program)

type W m = (Applicative m, MonadWriter [Name] m)

snu :: W m => Expression -> m ()
snu = \case
    Access  _ -> return ()
    Primary _ -> return ()
    Lambda p e1 -> do
        psnu p
        snu e1
    Beta e1 e2 -> do
        snu e1
        snu e2

psnu :: W m => Pattern -> m ()
psnu = \case
    PTuple p1 p2 -> do
        psnu p1
        psnu p2
    PAccess name _ -> do
        tell [name]
    PWildCard -> return ()
    PUnit -> return ()

fnsnu :: W m => Name -> Func -> m ()
fnsnu name fn = do
    tell [name]
    snu (view funcExpression fn)
