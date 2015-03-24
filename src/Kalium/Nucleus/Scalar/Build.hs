{-# LANGUAGE FlexibleInstances #-}

module Kalium.Nucleus.Scalar.Build where

import Kalium.Prelude
import Kalium.Nucleus.Scalar.Program

class LiftExpression a where
    expression :: a -> Expression

instance LiftExpression Atom    where expression = Atom
instance LiftExpression Literal where expression = expression . Primary
instance LiftExpression Name    where expression = expression . Access

class LiftStatement f where
    statement :: f config -> Statement config

instance LiftStatement Statement where statement = id
instance LiftStatement Exec      where statement = Execute
instance LiftStatement ForCycle  where statement = ForStatement
instance LiftStatement If        where statement = IfStatement
instance Scoping v => LiftStatement (Scope v Statement)
    where statement = ScopeStatement

statements :: (Foldable c, LiftStatement f) => c (f config) -> Statement config
statements ss = follow (map statement $ toList ss)

follow :: [Statement config] -> Statement config
follow = foldr Follow Pass
