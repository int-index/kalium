{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Purify where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M
import qualified Data.Set as S

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name

import Sodium.Nucleus.Vector.Show ()
import Debug.Trace

purify :: (Applicative m, MonadNameGen m) => EndoKleisli' m Program
purify program = do
    p1s <- execWriterT $ itraverse funcPurify (program ^. programFuncs)
    let p2s = resolve p1s
        infoGroups = fulfil p2s
    traceShow infoGroups (return (foldr substitute program infoGroups))

substitute :: [PurifyInfo] -> Endo' Program
substitute infoGroup program =
    let impureNames = S.unions [ S.singleton name | PurifyInfo name _ _ _ <- infoGroup ]
        program' = withPure
        dangling = program' `mentions` impureNames
        withoutImpure = S.foldr
            (\name -> programFuncs %~ M.delete name) program impureNames
        replacedCalls = withoutImpure & over recmapped purifyExpression
        withPure = replacedCalls & programFuncs %~ M.union pureFuncs
        pureFuncs = M.unions [ M.singleton name' func | PurifyInfo _ _ name' func <- infoGroup ]
    in if dangling then program else program'
  where
    purifyExpression :: Endo' Expression
    purifyExpression = foldr (.) id $ map (tryApply.appPurify) infoGroup

data Request = Request Name Int
    deriving (Eq, Ord)
data P1 = P1 Name Int Name (Map Name Name -> Maybe (Func, Set Request))
data P2 = P2 Name Int Name Func (Set Request)

data PurifyInfo = PurifyInfo Name Int Name Func

instance Show PurifyInfo where
    show (PurifyInfo name arity name' _) =
        show name ++ "[" ++ show arity ++ "] -> " ++ show name'

resolve :: [P1] -> [P2]
resolve p1s =
    let cell (P1 name _ name' _) = M.singleton name name'
        table = p1s & map cell & M.unions
    in p1s >>= resolve1 table

resolve1 :: Map Name Name -> P1 -> [P2]
resolve1 table (P1 name arity name' gen)
    = maybeToList $ uncurry (P2 name arity name') `fmap` gen table

-- TODO: recursive groups
fulfil :: [P2] -> [[PurifyInfo]]
fulfil p2s = [p2s >>= fulfil1] where
    -- FIXME: this algorithm does NOT work properly
    --        and sometimes results in groups with unsatisfied requests
    cell (P2 _ arity name' _ _) = S.singleton (Request name' arity)
    table = S.unions (map cell p2s)
    satisfied reqs = reqs `S.isSubsetOf` table
    fulfil1 (P2 name arity name' func reqs)
        | satisfied reqs = [PurifyInfo name arity name' func]
        | otherwise = []

funcPurify
    :: ( Applicative m
       , MonadNameGen m
       , MonadWriter [P1] m )
    => Name -> Func -> m ()
funcPurify name (Func ty a) = do
    name' <- alias name
    let (ps, b) = unlambda a
        arity = length ps
    case tyPurify arity ty of
        Just ty' | name /= NameSpecial OpMain -> do
            let b' = expForcePurify' b
                func' = b' & mapped . mapped . _1 %~ (Func ty' . lambda ps)
            tell [P1 name arity name' func']
        _ -> return ()

expForcePurify
    :: ( Applicative m
       , MonadReader (Map Name Name) m
       , MonadError () m
       , MonadWriter (Set Request) m)
    => EndoKleisli' m Expression
expForcePurify = \case
    Taint a -> return a
    Follow p x a -> Into p <$> expForcePurify x <*> expForcePurify a
    Into p x a -> Into p x <$> expForcePurify a
    AppOp3 OpIf xElse xThen cond
         -> AppOp3 OpIf
        <$> expForcePurify xElse
        <*> expForcePurify xThen
        <*> pure cond
    (unbeta -> (Access name : es)) -> do
        let arity = length es
        name' <- asks (M.lookup name) >>= maybe (throwError ()) return
        tell (S.singleton (Request name' arity))
        return (beta (Access name' : es))
    _ -> throwError ()

expForcePurify'
    :: Expression -> Map Name Name -> Maybe (Expression, Set Request)
expForcePurify' = fmap runError . runReaderT . runWriterT . expForcePurify
    where runError = either (const Nothing) Just . runExcept

tyPurify :: Int -> Type -> Maybe Type
tyPurify 0 (TypeTaint ty) = Just ty
tyPurify n (TypeFunction ty1 ty2)
    = TypeFunction ty1 <$> tyPurify (pred n) ty2
tyPurify _ _ = Nothing

appPurify :: PurifyInfo -> Expression -> Maybe Expression
appPurify (PurifyInfo name arity name' _) e
    | (Access op:es) <- unbeta e, op == name
    , arity == length es
    = Just $ Taint . beta $ (Access name' : es)
appPurify _ _ = Nothing
