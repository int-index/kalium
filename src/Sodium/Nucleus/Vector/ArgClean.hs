{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.ArgClean where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name

argClean :: (Applicative m, MonadNameGen m) => EndoKleisli' m Program
argClean program = do
    let funcs = program ^. programFuncs
    info <- execWriterT (itraverse funcArgClean funcs)
    return (sofar substitute info program)

substitute :: CleanInfo -> EndoKleisli' Maybe Program
substitute info@(CleanInfo name _ _ _) program
    | program' `mentions` name = Nothing
    | otherwise = Just program'
  where
    program'
        = programReplaceFunc info program
        & recmapped %~ tryApply (appArgClean info)

data CleanInfo = CleanInfo Name [Bool] Name Func

programReplaceFunc :: CleanInfo -> Endo' Program
programReplaceFunc (CleanInfo name _  name' func)
    = programFuncs %~ M.insert name' func . M.delete name

funcArgClean
    :: (Applicative m, MonadWriter [CleanInfo] m, MonadNameGen m)
    => Name -> Func -> m ()
funcArgClean name (Func ty a) = do
    name' <- alias name
    let (ps, b) = unlambda a
        (ns, ps') = patsArgClean ps
        a' = lambda ps' b
    case tyArgClean ns ty of
        Just ty' | ty' /= ty
          -> tell [CleanInfo name ns name' (Func ty' a')]
        _ -> return ()

patsArgClean :: [Pattern] -> ([Bool], [Pattern])
patsArgClean [] = ([], [])
patsArgClean (p:ps) =
    let (ns, ps') = patsArgClean ps
    in case p of
        PWildCard -> (False:ns, ps')
        _ -> (True:ns, p:ps')

tyArgClean :: [Bool] -> Type -> Maybe Type
tyArgClean [] ty = Just ty
tyArgClean (n:ns) (TypeApp2 TypeFunction ty1 ty2) = wrap <$> tyArgClean ns ty2
    where wrap | n = TypeApp2 TypeFunction ty1
               | otherwise = id
tyArgClean _ _ = Nothing

appArgClean :: CleanInfo -> Expression -> Maybe Expression
appArgClean (CleanInfo name ns name' _) e
    | (Access op:es) <- unbeta e, op == name
     = beta <$> ((Access name':) <$> zipFilter ns es)
appArgClean _ _ = Nothing
