{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.ArgClean where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name

argClean :: (Applicative m, MonadRename Integer String m) => Kleisli' m Program Program
argClean program = do
    let funcs = program ^. programFuncs
    info <- execWriterT (itraverse funcArgClean funcs)
    return (foldr substitute program info)

substitute :: CleanInfo -> Endo' Program
substitute info program
    | program' <- (recmapped %~ tryApply (appArgClean info))
                  (programReplaceFunc info program)
    , CleanInfo name _ _ _ <- info
    , False <- program' `mentions` name
    = program'
substitute _ program = program

data CleanInfo = CleanInfo Name [Bool] Name Func

programReplaceFunc :: CleanInfo -> Endo' Program
programReplaceFunc (CleanInfo name _  name' func)
    = programFuncs %~ M.insert name' func . M.delete name

alias :: (Applicative m, MonadRename Integer String m) => Name -> m Name
alias (NameGen m) = NameGen <$> rename m
alias _ = NameGen <$> mkname Nothing

funcArgClean :: (Applicative m, MonadWriter [CleanInfo] m, MonadRename Integer String m)
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
tyArgClean (n:ns) (TypeFunction ty1 ty2) = wrap <$> tyArgClean ns ty2
    where wrap | n = TypeFunction ty1
               | otherwise = id
tyArgClean _ _ = Nothing

appArgClean :: CleanInfo -> Expression -> Maybe Expression
appArgClean (CleanInfo name ns name' _) e
    | (Access op:es) <- unbeta e, op == name
     = beta <$> ((Access name':) <$> zipFilter ns es)
appArgClean _ _ = Nothing
