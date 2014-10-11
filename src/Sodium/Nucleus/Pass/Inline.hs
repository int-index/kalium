{-# LANGUAGE DataKinds #-}
module Sodium.Nucleus.Pass.Inline (inline) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Lens hiding (Index, Fold)
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

inline :: Program -> Program
inline = over recmapped inlineBody

inlineBody :: Body -> Body
inlineBody = evalState unconsBind where
    unconsBind :: State Body Body
    unconsBind = uses bodyBinds uncons >>= maybe get go
    go (bind, binds) = do
        bodyBinds .= binds
        elim bind <$> get >>= \case
            Just body -> put body >> unconsBind
            Nothing   -> over bodyBinds (bind:) <$> unconsBind
    elim bind |  bind ^. bindPattern   . to (==PWildCard)
              && bind ^. bindStatement . to noExec
              = return
    elim bind = \body' -> do
        Bind (PAccess name i) (Assign expr) <- return bind
        let (body, count)
              = runWriter
              $ flip runReaderT ((name, i), expr)
              $ inl body'
        when (expr ^? _Access == Nothing) $ guard (count <= 1)
        return body

-- check if a statement contains any side-effects
noExec :: Statement -> Bool
noExec = r where
    r = maybe False (const True) . recmapped failExec
    failExec :: Statement -> Maybe Statement
    failExec = \case
        Execute _ _ -> Nothing
        statement -> Just statement


type InlineEnv = ((Name, Index), Expression)

class Inline a where
    inl :: a -> ReaderT InlineEnv (Writer (Sum Integer)) a

instance Inline a => Inline [a] where
    inl = traverse inl

instance (Inline a, Inline b) => Inline (a, b) where
    inl = _1 inl >=> _2 inl

instance (Inline a, Inline b, Inline c) => Inline (a, b, c) where
    inl = _1 inl >=> _2 inl >=> _3 inl

instance Inline Name where
    inl = return

instance Inline Expression where
    inl expr = recInline expr >>= \case
        expr'@(Access name' j) -> do
            (name, expr) <- ask
            if name == (name', j)
                then tell (Sum 1) >> return expr
                else return expr'
        expr' -> return expr'
        where recInline
                =  _Tuple inl
               >=> _Call  inl
               >=> _Fold  inl
               >=> _MultiIfExpression inl

instance Inline Statement where
    inl  =  _Execute inl
        >=> _Assign  inl
        >=> _BodyStatement    inl
        >=> _ForStatement     inl
        >=> _MultiIfStatement inl

instance Inline Bind where
    inl = bindStatement inl

instance Inline ForCycle where
    inl  =  forRange   inl
        >=> forArgExpr inl
        >=> forAction  inl

instance Inline a => Inline (MultiIf a) where
    inl = multiIfLeafs inl

instance Inline Body where
    inl = bodyBinds inl >=> bodyResult inl
