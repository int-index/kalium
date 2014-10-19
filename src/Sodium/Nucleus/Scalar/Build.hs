{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Sodium.Nucleus.Scalar.Build where

import Data.Foldable
import Sodium.Nucleus.Scalar.Program

class LiftLiteral a where
    literal :: a -> Literal

instance LiftLiteral Literal  where literal = id
instance LiftLiteral Integer  where literal = LitInteger
instance LiftLiteral Rational where literal = LitDouble
instance LiftLiteral Bool     where literal = LitBoolean
instance LiftLiteral String   where literal = LitString
instance LiftLiteral ()       where literal = const LitUnit

class LiftAtom a where
    atom :: a -> Atom

instance LiftAtom Atom where atom = id
instance LiftAtom Name where atom = Access
instance LiftLiteral a => LiftAtom a where atom = Primary . literal

expression :: LiftAtom a => a -> Expression
expression = Atom . atom

class LiftStatement f where
    statement :: f a -> Statement a

instance LiftStatement Statement where statement = id
instance LiftStatement Exec      where statement = Execute
instance LiftStatement ForCycle  where statement = ForStatement
instance LiftStatement If        where statement = IfStatement
instance LiftStatement Scope     where statement = ScopeStatement

statements :: (Foldable c, LiftStatement f) => c (f a) -> Statement a
statements ss = Group (map statement $ toList ss)

assign :: Name -> a -> Statement a
assign name a = statement $ Exec (Just name) (NameOp OpId) [a]
