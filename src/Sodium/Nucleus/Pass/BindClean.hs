{-# LANGUAGE FlexibleInstances #-}
module Sodium.Nucleus.Pass.BindClean (bindClean) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Tuple

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap
import Sodium.Util (Pairs)

bindClean :: Program -> Program
bindClean = over recmapped bindCleanBody

bindCleanBody :: Body Statement -> Body Statement
bindCleanBody = bodyBinds . traversed %~ bindCleanBind

bindCleanBind :: Bind Statement -> Bind Statement
bindCleanBind bind
    = maybe bind id
    $ msum
    $ map (subst bind)
    $ patClean (bind ^. bindPattern)

subst :: Bind Statement -> (Pattern, Cleaner) -> Maybe (Bind Statement)
subst bind (pat, cleaner)
    = bind & bindPattern .~ pat
           & bindStatement (cleanRet cleaner)

type Con = forall a . (a,a) -> (a,a)

patClean :: Pattern -> Pairs Pattern Cleaner
patClean p@PWildCard = return (p, wildCleaner)
patClean (PTuple pat1 pat2) = go id OpFst `mplus` go swap OpSnd where
    go :: Con -> Operator -> Pairs Pattern Cleaner
    go con op = do
        let (p1, p2) = con (pat1, pat2)
        let wrap (PWildCard, _) = do
                let cln expr = return $ Call (OpAccess op) expr
                return (p1, cln)
            wrap (p0, cln') = do
                let cln (Call2 (OpAccess OpPair) expr1 expr2) = do
                      let (act1, act2) = con (pure, cln')
                      Call2 (OpAccess OpPair) <$> act1 expr1 <*> act2 expr2
                    cln _ = Nothing
                return (PTuple `uncurry` con (p1, p0), cln)
        patClean p2 >>= wrap
patClean _ = []

type Cleaner = Expression -> Maybe Expression

wildCleaner :: Cleaner
wildCleaner _ = Just (Primary (Lit STypeUnit ()))

class CleanRet a where
    cleanRet :: Cleaner -> a -> Maybe a

instance CleanRet Expression where
    cleanRet = id

instance CleanRet (Body Statement) where
    cleanRet cc = bodyResult (cleanRet cc)

instance CleanRet (MultiIf Statement) where
    cleanRet cc  = (multiIfLeafs . traversed . _2) (cleanRet cc)

instance CleanRet ForCycle where
    -- even if the value is not
    -- used outside the loop, it
    -- still needs to be passed
    cleanRet _cc = const mzero

instance CleanRet (Lambda Statement) where
    cleanRet cc = lamAction (cleanRet cc)

instance CleanRet Statement where
    cleanRet cc = \case
        Assign  a -> Assign <$> cleanRet cc a
        Execute _ -> mzero
        ForStatement     a -> ForStatement     <$> cleanRet cc a
        MultiIfStatement a -> MultiIfStatement <$> cleanRet cc a
        BodyStatement    a -> BodyStatement    <$> cleanRet cc a
        LambdaStatement  a -> LambdaStatement  <$> cleanRet cc a
