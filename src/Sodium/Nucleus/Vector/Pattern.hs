module Sodium.Nucleus.Vector.Pattern where

import qualified Data.Set as S

import Sodium.Nucleus.Vector.Program

patBound :: Pattern -> S.Set Name
patBound = \case
    PWildCard -> S.empty
    PUnit -> S.empty
    PAccess name _ -> S.singleton name
    PTuple p1 p2 -> patBound p1 `S.union` patBound p2

preciseMatch :: Pattern -> Expression -> Bool
preciseMatch = \case
    PWildCard -> const False
    PUnit -> (==) (Literal STypeUnit ())
    PAccess name1 _ -> \case
        Atom (Access name2) | name1 == name2 -> True
        _ -> False
    PTuple p1 p2 -> \case
        App2 (OpAccess OpPair) e1 e2 -> preciseMatch p1 e1 && preciseMatch p2 e2
        _ -> False
