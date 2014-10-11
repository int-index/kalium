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
              $ subOnce body'
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


type SubOnceEnv = ((Name, Index), Expression)

class SubOnce a where
	subOnce :: a -> ReaderT SubOnceEnv (Writer (Sum Integer)) a

instance SubOnce Expression where
    subOnce = \case
        Primary prim -> return (Primary prim)
        Access name' j -> do
            (name, expr) <- ask
            if name == (name', j)
                then tell (Sum 1) >> return expr
                else return (Access name' j)
        Tuple exprs
             -> Tuple
            <$> traversed subOnce exprs
        Call op exprs
             -> Call op
            <$> traversed subOnce exprs
        Fold op expr range
             -> Fold op
            <$> subOnce expr
            <*> subOnce range
        MultiIfExpression multiIf
             -> MultiIfExpression
            <$> subOnce multiIf

instance SubOnce Statement where
	subOnce
		 = (_Execute . _2 . traversed) subOnce
		>=> _Assign subOnce
		>=> _BodyStatement    subOnce
		>=> _ForStatement     subOnce
		>=> _MultiIfStatement subOnce

instance SubOnce Bind where
	subOnce = bindStatement subOnce

instance SubOnce ForCycle where
	subOnce
		 =  forRange subOnce
		>=> forArgExpr subOnce
		>=> (forAction subOnce)

instance SubOnce a => SubOnce (MultiIf a) where
    subOnce = (multiIfLeafs . traversed) (_1 subOnce >=> _2 subOnce)

instance SubOnce Body where
    subOnce
         = (bodyBinds . traversed) subOnce
        >=> bodyResult subOnce
