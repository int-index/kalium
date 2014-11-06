{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Sodium.Nucleus.Scalar.Build where

import Data.Foldable
import Sodium.Nucleus.Scalar.Program

class LiftLiteral a where
    literal :: a -> Literal

instance LiftLiteral Literal where
    literal = id
instance LiftLiteral Integer  where literal = Lit STypeInteger
instance LiftLiteral Rational where literal = Lit STypeDouble
instance LiftLiteral Bool     where literal = Lit STypeBoolean
instance LiftLiteral Char     where literal = Lit STypeChar
instance LiftLiteral String   where literal = Lit (STypeList STypeChar)
instance LiftLiteral ()       where literal = Lit STypeUnit

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
instance Scoping v => LiftStatement (Scope v Statement)
    where statement = ScopeStatement

statements :: (Foldable c, LiftStatement f) => c (f a) -> Statement a
statements ss = Group (map statement $ toList ss)

assign :: Name -> a -> Statement a
assign name a = statement $ Exec (PAccess name) (NameOp OpId) [a]
