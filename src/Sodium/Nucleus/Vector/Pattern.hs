{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Pattern where

import qualified Data.Set as S

import Sodium.Prelude
import Sodium.Nucleus.Vector.Program

patBound :: Pattern -> Set Name
patBound = \case
    PWildCard -> mempty
    PUnit -> mempty
    PAccess name _ -> S.singleton name
    PTuple p1 p2 -> patBound p1 <> patBound p2

patRemoveUnits :: MonadWriter (Set Name) m => Pattern -> m Pattern
patRemoveUnits = \case
    PWildCard -> return PWildCard
    PUnit -> return PWildCard
    PAccess name TypeUnit -> do
        tell (S.singleton name)
        return PWildCard
    p@(PAccess _ _) -> return p
    PTuple p1 p2 -> liftM2 PTuple (patRemoveUnits p1) (patRemoveUnits p2)

patType :: Pattern -> Maybe Type
patType = \case
    PWildCard -> Nothing
    PUnit -> return TypeUnit
    PAccess _ ty -> return ty
    PTuple p1 p2 -> liftM2 TypePair (patType p1) (patType p2)

patIsAccess (PAccess _ _) = True
patIsAccess _ = False

preciseMatch :: Pattern -> Expression -> Bool
preciseMatch = \case
    PWildCard -> const False
    PUnit -> (==) LitUnit
    PAccess name1 _ -> \case
        Access name2 | name1 == name2 -> True
        _ -> False
    PTuple p1 p2 -> \case
        App2 (OpAccess OpPair) e1 e2 -> preciseMatch p1 e1 && preciseMatch p2 e2
        _ -> False
