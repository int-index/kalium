{-# LANGUAGE FlexibleInstances #-}
module Sodium.Nucleus.Pass.BindClean (bindClean) where

import Control.Monad
import Control.Lens
import Data.Monoid
import Data.List

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

bindClean :: Program -> Program
bindClean = over recmapped bindCleanBody

bindCleanBody :: Body -> Body
bindCleanBody = bodyBinds . traversed %~ bindCleanBind

bindCleanBind :: Bind Statement -> Bind Statement
bindCleanBind bind
    = maybe bind id
    $ getFirst . mconcat . map First
    $ map (subst bind)
    $ patClean (bind ^. bindPattern)

subst :: Bind Statement -> (Pattern, Cleaner) -> Maybe (Bind Statement)
subst bind (pat, cleaner)
    = bind & bindPattern .~ pat
           & bindStatement (cleanRet cleaner)

patClean :: Pattern -> [(Pattern, Cleaner)]
patClean p@PWildCard = [(p, wildCleaner)]
patClean (PTuple pats) = concat (unfoldr go 0) where
    go n = do
        (pre, cur:post) <- Just (splitAt n pats)
        let wrap (PWildCard, _) = [(PTuple (pre++post), tupWildCleaner pats n)]
            wrap (cur', cln') = [(PTuple (pre++cur':post), tupNestedCleaner cln' n)]
        return (patClean cur >>= wrap, succ n)
patClean _ = []

type Cleaner = Expression -> Maybe Expression

wildCleaner :: Cleaner
wildCleaner _ = Just (Primary (DLit STypeUnit ()))

tupWildCleaner :: [Pattern] -> Int -> Cleaner
tupWildCleaner pats n expr = getFirst (First direct <> First indirect)
 where direct = do
         Tuple  exprs  <- Just expr
         (pre, _:post) <- Just (splitAt n exprs)
         return $ Tuple (pre ++ post)
       indirect = do
         guard $ length pats == 2
         op <- case n of
            0 -> Just OpSnd
            1 -> Just OpFst
            _ -> Nothing
         return $ Call (NameOp op) [expr]

tupNestedCleaner :: Cleaner -> Int -> Cleaner
tupNestedCleaner cln n expr = do
    Tuple exprs <- Just expr
    (pre, cur:post) <- Just (splitAt n exprs)
    cur' <- cln cur
    return $ Tuple (pre ++ cur':post)

class CleanRet a where
    cleanRet :: Cleaner -> a -> Maybe a

instance CleanRet Expression where
    cleanRet = id

instance CleanRet Body where
    cleanRet cc = bodyResult (cleanRet cc)

instance CleanRet (MultiIf Statement) where
    cleanRet cc  = (multiIfLeafs . traversed . _2) (cleanRet cc)

instance CleanRet (ForCycle Statement) where
    -- even if the value is not
    -- used outside the loop, it
    -- still needs to be passed
    cleanRet cc = const mzero

instance CleanRet Statement where
    cleanRet cc
         =  _Assign  (cleanRet cc)
        >=> _Execute (const mzero)
        >=> _ForStatement     (cleanRet cc)
        >=> _MultiIfStatement (cleanRet cc)
        >=> _BodyStatement    (cleanRet cc)
