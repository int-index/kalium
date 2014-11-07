{-# LANGUAGE GADTs #-}
module Sodium.Nucleus.Pattern where

import Data.Traversable
import Sodium.Nucleus.Program.Vector

patBound :: Pattern -> [Name]
patBound (PTuple xs) = xs >>= patBound
patBound (PAccess name _) = [name]
patBound _ = []

-- The expression contains all the information
-- the pattern needs
patMatch :: Pattern -> Expression -> Bool
patMatch PWildCard _ = True
patMatch PUnit (Primary (Lit STypeUnit ())) = True
patMatch (PTuple xs) (Tuple ys)
    =  length xs == length ys
    && and (zipWith patMatch xs ys)
patMatch (PAccess name1 i) (Access name2 j)
    = name1 == name2 && i == j
patMatch _ _ = False

-- The pattern contains all the information
-- the expression needs
expMatch :: Pattern -> Expression -> Bool
expMatch PWildCard _ = False
expMatch PUnit (Primary (Lit STypeUnit ())) = True
expMatch (PTuple xs) (Tuple ys)
    =  length xs == length ys
    && and (zipWith expMatch xs ys)
expMatch (PAccess name1 i) (Access name2 j)
    = name1 == name2 && i == j
expMatch _ _ = False
