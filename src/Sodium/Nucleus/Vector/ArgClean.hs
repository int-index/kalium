{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.ArgClean where

import Control.Applicative
import Control.Lens
import Control.Monad.Supply
import Control.Monad.Writer

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name
import Sodium.Util

argClean :: (Applicative m, MonadSupply Integer m) => Program -> m Program
argClean program = do
    let funcs = program ^. programFuncs
    info <- execWriterT (traverse funcArgClean funcs)
    return (foldr substitute program info)

substitute :: CleanInfo -> Program -> Program
substitute info program
    | program' <- (recmapped %~ tryApply (appArgClean info))
                  (programReplaceFunc info program)
    , CleanInfo name _ _ _ <- info
    , False <- program' `mentions` name
    = program'
substitute _ program = program

data CleanInfo = CleanInfo Name [Bool] Bool Func

programReplaceFunc :: CleanInfo -> Program -> Program
programReplaceFunc (CleanInfo name _ _ func) = programFuncs %~ map replace
  where
    replace (view funcName -> name') | name == name' = func
    replace func = func

funcArgClean :: (Applicative m, MonadWriter [CleanInfo] m, MonadSupply Integer m) => Func -> m ()
funcArgClean (Func ty name a) = do
    name' <- NameGen <$> supply
    let (ps, b) = unlambda a
        (ns, ps') = patsArgClean ps
        (untaint, b') = case b of
            Taint e | name /= NameSpecial OpMain -> (True, e)
            e -> (False, e)
        a' = lambda ps' b'
    case tyArgClean ns untaint ty of
        Just ty' | ty' /= ty
          -> tell [CleanInfo name ns untaint (Func ty' name' a')]
        _ -> return ()

patsArgClean :: [Pattern] -> ([Bool], [Pattern])
patsArgClean [] = ([], [])
patsArgClean (p:ps) =
    let (ns, ps') = patsArgClean ps
    in case p of
        PWildCard -> (False:ns, ps')
        _ -> (True:ns, p:ps')

tyArgClean :: [Bool] -> Bool -> Type -> Maybe Type
tyArgClean [] False ty = Just ty
tyArgClean [] True (TypeTaint ty) = Just ty
tyArgClean (n:ns) untaint (TypeFunction ty1 ty2) = wrap <$> tyArgClean ns untaint ty2
    where wrap | n = TypeFunction ty1
               | otherwise = id
tyArgClean _ _ _ = Nothing

appArgClean :: CleanInfo -> Expression -> Maybe Expression
appArgClean (CleanInfo name ns untaint func) e
    | name' <- view funcName func
    , (Access op:es) <- unbeta e
    , op == name
     = (if untaint then Taint else id) . foldl1 Beta
    <$> ((Access name':) <$> zipFilter ns es)
appArgClean _ _ = Nothing
