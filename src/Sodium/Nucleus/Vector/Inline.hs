{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Inline where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Set as S

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Pattern
import Sodium.Nucleus.Vector.Name
import Sodium.Nucleus.Vector.Cost

inline :: (Applicative m, MonadRename Integer String m, Recmappable a) => EndoKleisli' m a
inline = recmapped (mergePattern . inlineExpression)

reorder :: (Applicative m, MonadRename Integer String m, Recmappable a) => EndoKleisli' m a
reorder = recmapped reorderPattern

inlineExpression (Into p x a) | not excessive, not dangling = b
    where (b, count) = runWriter (recmapped w a)
          w = replace $ \case
            e | preciseMatch p e -> Just x
            _ -> Nothing
          excessive = case x of
            Access _ -> False
            OpAccess OpUnit -> False
            _ -> getSum count > 1
          dangling = b `mentions` patBound p
inlineExpression (Lambda p a)
    | (p', names) <- runWriter (patRemoveUnits p)
    , not (S.null names)
    = let (b, _) = runWriter (recmapped w a)
          w = replace $ \case
            Access name | name `S.member` names -> Just (OpAccess OpUnit)
            _ -> Nothing
      in (Lambda p' b)
inlineExpression e = e

mergePattern e@(Lambda p a)
    | Just ty <- patType p
    , not (patIsAccess p)
    = do
        name <- NameGen <$> mkname Nothing
        let w = replace $ \case
              e | preciseMatch p e -> Just (Access name)
              _ -> Nothing
            (b, _) = runWriter (recmapped w a)
            dangling = b `mentions` patBound p
        return (if dangling then e else Lambda (PAccess name ty) b)
mergePattern e = return e

reorderPattern e | Follow p x a <- e = do
    rs <- for (reorders p x) $ \(p', x') -> do
        Follow p' <$> inline x' <*> pure a
    return (lesser e rs)
reorderPattern e = return e

replace :: MonadWriter (Sum Integer) m => EndoKleisli' Maybe Expression -> EndoKleisli' m Expression
replace fits e
    | Just e' <- fits e = tell (Sum 1) >> return e'
    | otherwise = return e

reorderings :: Expression -> Pairs (Pattern -> Maybe Pattern) Expression
reorderings = \case
    AppOp2 OpPair x y -> do
        (fx, x') <- reorderings x
        (fy, y') <- reorderings y
        let r1 = (pairMatch id   fx fy, AppOp2 OpPair x' y')
            r2 = (pairMatch flip fx fy, AppOp2 OpPair y' x')
        [r1, r2]
    e -> [(Just, e)]
  where
    pairMatch k fx fy = \case
        PTuple x y -> k PTuple <$> fx x <*> fy y
        _ -> Nothing

reorders :: Pattern -> Expression -> Pairs Pattern Expression
reorders p (Taint a) = do
    (f, b) <- reorderings a
    case f p of
        Nothing -> []
        Just p' -> [(p', Taint b)]
reorders p (Follow p_ x_ a) = do
    (p', b) <- reorders p a
    return (p', Follow p_ x_ b)
reorders _ _ = []
