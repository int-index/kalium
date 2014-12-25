{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Sodium.Nucleus.Scalar.Build where

import Sodium.Prelude
import Sodium.Nucleus.Scalar.Program

class LiftExpression a where
    expression :: a -> Expression

instance LiftExpression Atom    where expression = Atom
instance LiftExpression Literal where expression = expression . Primary
instance LiftExpression Name    where expression = expression . Access

instance LiftExpression () where expression () = Call (NameSpecial OpUnit) [] []
instance LiftExpression Bool where
    expression = \case
        True  -> Call (NameSpecial OpTrue)  [] []
        False -> Call (NameSpecial OpFalse) [] []
instance LiftExpression String where
    expression = foldr listCons listNil . map expression
        where listNil = Call (NameSpecial OpNil) [TypeChar] []
              listCons x xs = Call (NameSpecial OpCons) [] [x, xs]


instance LiftExpression Integer  where expression = expression . LitInteger
instance LiftExpression Rational where expression = expression . LitDouble
instance LiftExpression Char     where expression = expression . LitChar

class LiftStatement f where
    statement :: f a p -> Statement a p

instance LiftStatement Statement where statement = id
instance LiftStatement Exec      where statement = Execute
instance LiftStatement ForCycle  where statement = ForStatement
instance LiftStatement If        where statement = IfStatement
instance Scoping v => LiftStatement (Scope v Statement)
    where statement = ScopeStatement

statements :: (Foldable c, LiftStatement f) => c (f a p) -> Statement a p
statements ss = follow (map statement $ toList ss)

follow :: [Statement a p] -> Statement a p
follow = foldr Follow Pass

assign :: Name -> a -> Statement Pattern a
assign name a = statement $ Exec (PAccess name) (NameSpecial OpId) [] [a]
