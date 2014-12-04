{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Sodium.Nucleus.Scalar.Build where

import qualified Data.List as L
import Data.Foldable
import Sodium.Nucleus.Scalar.Program

class LiftExpression a where
    expression :: a -> Expression

instance LiftExpression Atom    where expression = Atom
instance LiftExpression Literal where expression = expression . Primary
instance LiftExpression Name    where expression = expression . Access

instance LiftExpression () where expression () = Call (NameOp OpUnit) []
instance LiftExpression Bool where
    expression = \case
        True  -> Call (NameOp OpTrue)  []
        False -> Call (NameOp OpFalse) []

instance LiftExpression Integer  where expression = expression . Lit STypeInteger
instance LiftExpression Rational where expression = expression . Lit STypeDouble
instance LiftExpression Char     where expression = expression . Lit STypeChar
instance LiftExpression String   where expression = expression . Lit (STypeList STypeChar)


class LiftStatement f where
    statement :: f a p -> Statement a p

instance LiftStatement Statement where statement = id
instance LiftStatement Exec      where statement = Execute
instance LiftStatement ForCycle  where statement = ForStatement
instance LiftStatement If        where statement = IfStatement
instance Scoping v => LiftStatement (Scope v Statement)
    where statement = ScopeStatement

statements :: (Foldable c, LiftStatement f) => c (f a p) -> Statement a p
statements ss = group (map statement $ toList ss)

group :: [Statement a p] -> Statement a p
group = L.foldr Follow Pass

assign :: Name -> a -> Statement Pattern a
assign name a = statement $ Exec (PAccess name) (NameOp OpId) [a]
