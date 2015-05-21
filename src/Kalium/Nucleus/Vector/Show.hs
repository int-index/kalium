{-# LANGUAGE FlexibleInstances #-}
module Kalium.Nucleus.Vector.Show where

import Kalium.Prelude
import Kalium.Nucleus.Vector.Program

deriving instance Show NameSpecial
deriving instance Show Literal

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
        Ext ext -> absurd ext

instance Show Pattern where
    show = \case
        PUnit -> "()"
        PWildCard -> "_"
        PAccess name _ -> show name
        PTuple p1 p2 -> "(" ++ show p1 ++ "," ++ show p2 ++ ")"
        PExt pext -> absurd pext

deriving instance Show Type
deriving instance Show Func
deriving instance Show Program
