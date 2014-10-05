module Sodium.Nucleus.Pattern where

import Sodium.Nucleus.Program.Vector

patBound :: Pattern -> [Name]
patBound (PTuple xs) = xs >>= patBound
patBound (PAccess name _) = [name]
patBound _ = []

patMatch :: Pattern -> Expression -> Bool
patMatch (PTuple xs) (Tuple ys)
    =  length xs == length ys
    && and (zipWith patMatch xs ys)
patMatch (PAccess name1 i) (Access name2 j)
    = name1 == name2 && i == j
patMatch PWildCard _ = True
patMatch _ _ = False
