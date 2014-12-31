{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Purify where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name

purify :: (Applicative m, MonadNameGen m) => EndoKleisli' m Program
purify program = do
    let funcs = program ^. programFuncs
    info <- execWriterT (itraverse funcPurify funcs)
    return (foldr substitute program info)

substitute :: CleanInfo -> Endo' Program
substitute info program
    | program' <- (recmapped %~ tryApply (appPurify info))
                  (programReplaceFunc info program)
    , CleanInfo name _ _ _ <- info
    , False <- program' `mentions` name
    = program'
substitute _ program = program

data CleanInfo = CleanInfo Name Int Name Func

programReplaceFunc :: CleanInfo -> Endo' Program
programReplaceFunc (CleanInfo name _  name' func)
    = programFuncs %~ M.insert name' func . M.delete name

funcPurify :: (Applicative m, MonadWriter [CleanInfo] m, MonadNameGen m)
             => Name -> Func -> m ()
funcPurify name (Func ty a) = do
    name' <- alias name
    let (ps, b) = unlambda a
        n = length ps
        mb' = case b of
            Taint e | name /= NameSpecial OpMain -> Just e
            _ -> Nothing
        ma' = lambda ps <$> mb'
    case tyPurify n ty of
        Just ty' | ty' /= ty
                 , Just a' <- ma'
          -> tell [CleanInfo name n name' (Func ty' a')]
        _ -> return ()

tyPurify :: Int -> Type -> Maybe Type
tyPurify 0 (TypeTaint ty) = Just ty
tyPurify n (TypeFunction ty1 ty2)
    = TypeFunction ty1 <$> tyPurify (pred n) ty2
tyPurify _ _ = Nothing

appPurify :: CleanInfo -> Expression -> Maybe Expression
appPurify (CleanInfo name n name' _) e
    | (Access op:es) <- unbeta e, op == name
    , n == length es
    = Just $ Taint . beta $ (Access name' : es)
appPurify _ _ = Nothing
