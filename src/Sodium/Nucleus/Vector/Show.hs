module Sodium.Nucleus.Vector.Show where

import Sodium.Nucleus.Vector.Program

instance Show Name where
    show = \case
        NameSpecial op -> show op
        NameGen n -> "_" ++ show n

instance Show Expression where
    show = \case
        AppOp2 OpPair x y -> "(" ++ show x ++ "," ++ show y ++ ")"
        Access name -> show name
        Primary lit -> show lit
        Lambda p a -> "(λ" ++ show p ++ "." ++ show a ++ ")"
        Beta a b -> show a ++ "(" ++ show b ++ ")"

instance Show Pattern where
    show = \case
        PUnit -> "()"
        PWildCard -> "_"
        PAccess name _ -> show name
        PTuple p1 p2 -> "(" ++ show p1 ++ "," ++ show p2 ++ ")"
