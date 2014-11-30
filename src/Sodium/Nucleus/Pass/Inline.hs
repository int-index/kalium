{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Pass.Inline (inline) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Supply
import qualified Data.Set as S
import Control.Lens hiding (Index, Fold)
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

inline :: (Applicative m, MonadSupply Name m) => Program -> m Program
inline = recmapped inlineBody

inlineBody :: (Applicative m, MonadSupply Name m) => Body Statement -> m (Body Statement)
inlineBody = evalStateT unconsBind where
    unconsBind :: (Applicative m, MonadSupply Name m) => StateT (Body Statement) m (Body Statement)
    unconsBind = uses bodyBinds uncons >>= maybe get go
    go (bind', binds) = do
        bodyBinds .= binds
        bind <- bindPattern merge bind'
        elim bind <$> get >>= \case
            Just body -> put body >> unconsBind
            Nothing   -> over bodyBinds (bind:) <$> unconsBind
    elim bind |  bind ^. bindPattern   . to (==PWildCard)
              && bind ^. bindStatement . to noExec
              = return
    elim bind = \body' -> do
        Bind (PAccess name _) (Assign expr) <- return bind
        let (body, count)
              = runWriter
              $ flip runReaderT (name, expr)
              $ recmapped inl body'
        case expr of
            Access _ -> return ()
            _ -> guard (count <= 1)
        return body
    merge :: (Applicative m, MonadSupply Name m) => Pattern -> StateT (Body Statement) m Pattern
    merge p@(PTuple pat1 pat2)
        | PAccess top1 ty1 <- pat1
        , PAccess top2 ty2 <- pat2
        = do top <- indexTag GlobalTag <$> supply
             let model =  Call2 (OpAccess OpPair) (Access top1) (Access top2)
             let pat   = PAccess top (TypePair ty1 ty2)
             let expr  =  Access top
             gets (over recmapped (model `subst` expr))
                >>= \case body | eb1 <- execWriter (body  & recmapped exprBound)
                               , eb2 <- execWriter (model & recExpr   exprBound)
                               , S.null (eb1 `S.intersection` eb2)
                            -> put body >> return pat
                          _ -> return p
    merge pat = return pat

subst :: Expression -> Expression -> (Expression -> Expression)
subst model expr expr'
    | model == expr' = expr
    | otherwise = expr'

exprBound :: Expression -> Writer (S.Set (Name1 IndexTag)) Expression
exprBound expr = do
    case expr of
        Access name -> tell (S.singleton name)
        _ -> return ()
    return expr


-- check if a statement contains any side-effects
noExec :: Statement -> Bool
noExec = r where
    r = maybe False (const True) . recmapped failExec
    failExec :: Statement -> Maybe Statement
    failExec = \case
        Execute _ -> Nothing
        statement -> Just statement

type Inline = ReaderT (Name1 IndexTag, Expression) (Writer (Sum Integer))

inl :: Expression -> Inline Expression
inl expr'@(Access name') = do
        (name, expr) <- ask
        if name == name'
            then tell (Sum 1) >> return expr
            else return expr'
inl expr' = return expr'
