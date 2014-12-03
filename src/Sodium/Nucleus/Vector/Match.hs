{-# LANGUAGE GADTs #-}
module Sodium.Nucleus.Vector.Match where

import Control.Lens

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name

match :: Program -> Program
match = over recmapped matchExpression

matchExpression :: Expression -> Expression
matchExpression = \case

    App2 (OpAccess OpBind) (OpAccess OpTaint `App` a) x -> App x a

    App2 (OpAccess OpBind) x (Lambda PWildCard a)
        -> App2 (OpAccess OpBindIgnore) x a

    App2 (OpAccess OpBindIgnore) x (OpAccess OpTaint `App` a)
        -> App2 (OpAccess OpFmapIgnore) a x

    App2 (OpAccess OpFmapIgnore) (Literal STypeUnit ()) x
        -> App (OpAccess OpIgnore) x

    App2 (OpAccess OpFmapIgnore) a (App (OpAccess OpTaint) _)
        -> App (OpAccess OpTaint) a

    App2 (OpAccess OpFmapIgnore) a (App (OpAccess OpIgnore) x)
        -> App2 (OpAccess OpFmapIgnore) a x

    App (OpAccess OpIgnore) (App (OpAccess OpTaint) _)
        -> App (OpAccess OpTaint) (Literal STypeUnit ())

    App (OpAccess OpIgnore) e@(App (OpAccess OpIgnore) _) -> e

    App2 (OpAccess OpBind) x (OpAccess OpTaint) -> x

    Lambda PWildCard a `App` _ -> a

    Lambda p1 (App f e1) | pessimisticMatch p1 e1 -> f

    Lambda pat a -> Lambda (cleanUsage a pat) a

    e -> e

cleanUsage :: Mask scope => scope -> Pattern -> Pattern
cleanUsage a = go where
    go = \case
        PUnit -> PWildCard
        PTuple p1 p2 -> PTuple (go p1) (go p2)
        PAccess name _ | not (checkRef a name) -> PWildCard
        p -> p

-- TODO: revive pattern-matching
pessimisticMatch :: Pattern -> Expression -> Bool
pessimisticMatch (PAccess name1 _) (Atom (Access name2)) | name1 == name2 = True
pessimisticMatch (PTuple p1 p2) (App2 (OpAccess OpPair) e1 e2)
    = pessimisticMatch p1 e1 && pessimisticMatch p2 e2
pessimisticMatch _ _ = False
