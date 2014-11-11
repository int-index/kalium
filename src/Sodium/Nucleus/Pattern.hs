{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Sodium.Nucleus.Pattern where

import Sodium.Nucleus.Program.Vector

patBound :: Pattern -> [Name]
patBound (PTuple x1 x2) = [x1, x2] >>= patBound
patBound (PAccess name _) = [name]
patBound _ = []

-- The expression contains all the information
-- the pattern needs
patMatch :: Pattern -> Expression -> Bool
patMatch PWildCard _ = True
patMatch PUnit (Primary (Lit STypeUnit ())) = True
patMatch (PTuple x1 x2) (CallOp2 OpPair y1 y2) = patMatch x1 y1 && patMatch x2 y2
patMatch (PAccess name1 i) (Access name2 j) = name1 == name2 && i == j
patMatch _ _ = False

-- The pattern contains all the information
-- the expression needs
expMatch :: Pattern -> Expression -> Bool
expMatch PWildCard _ = False
expMatch PUnit (Primary (Lit STypeUnit ())) = True
expMatch (PTuple x1 x2) (CallOp2 OpPair y1 y2) = expMatch x1 y1 && expMatch x2 y2
expMatch (PAccess name1 i) (Access name2 j) = name1 == name2 && i == j
expMatch _ _ = False
