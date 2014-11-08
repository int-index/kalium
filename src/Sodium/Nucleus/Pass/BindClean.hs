{-# LANGUAGE FlexibleInstances #-}
module Sodium.Nucleus.Pass.BindClean (bindClean) where

import Control.Applicative
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
patClean (PTuple pat1 pat2) = go OpFst ++ go OpSnd where
    go OpFst = do
        let wrap (PWildCard, _) = [(pat1, \expr -> return $ Call (NameOp OpFst) [expr])]
            wrap (cur', cln') = [(PTuple pat1 cur', tupNestedCleaner cln' False)]
        patClean pat2 >>= wrap
    go OpSnd = do
        let wrap (PWildCard, _) = [(pat2, \expr -> return $ Call (NameOp OpSnd) [expr])]
            wrap (cur', cln') = [(PTuple cur' pat2, tupNestedCleaner cln' True)]
        patClean pat1 >>= wrap
    go _ = []
patClean _ = []

type Cleaner = Expression -> Maybe Expression

wildCleaner :: Cleaner
wildCleaner _ = Just (Primary (Lit STypeUnit ()))

tupNestedCleaner :: Cleaner -> Bool -> Cleaner
tupNestedCleaner cln op expr = do
    Tuple expr1 expr2 <- Just expr
    if op then Tuple <$> cln expr1 <*> pure expr2
          else Tuple <$> pure expr1 <*> cln expr2

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
