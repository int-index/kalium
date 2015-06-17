{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Kalium.Nucleus.Scalar.Valueficate where

import Kalium.Prelude
import Control.Monad.Reader

import qualified Data.Map as M

import Kalium.Nucleus.Scalar.Program

class ValueficateSubstitute a where
    valueficateSubstitute
        :: (Applicative m, MonadReader ReferenceInfo m)
        =>    a (Configuration param0 Pattern Atom)
        -> m (a (Configuration param1 Pattern Expression))

ap1On f a   = f <$> valueficateSubstitute a
ap2On f a b = ap1On f a <*> valueficateSubstitute b

instance ValueficateSubstitute Exec where
    valueficateSubstitute (Exec ret op tyArgs args) = do
        referenceInfo <- ask
        let ret' = foldr1 PTuple (ret:rets)
            rets = case M.lookup op referenceInfo of
                Nothing -> []
                Just currentReferenceInfo -> do
                    (keep, arg) <- zip currentReferenceInfo args
                    guard keep
                    return $ case arg of
                        Access name -> PAccess name
                        Primary _ -> error "non-variables by-ref"
        return $ Exec ret' op tyArgs (map Atom args)

instance ValueficateSubstitute Statement where
    valueficateSubstitute = \case
        Follow a1 a2 -> ap2On Follow a1 a2
        ScopeStatement a -> ap1On ScopeStatement a
        IfStatement  a -> ap1On IfStatement a
        ForStatement a -> ap1On ForStatement a
        Execute a -> ap1On Execute a
        Pass -> return Pass

instance   ValueficateSubstitute obj
        => ValueficateSubstitute (Scope vars obj) where
    valueficateSubstitute = scopeElem valueficateSubstitute

instance ValueficateSubstitute If where
    valueficateSubstitute (If ifCond ifThen ifElse)
       = ap2On (If (Atom ifCond)) ifThen ifElse

instance ValueficateSubstitute ForCycle where
    valueficateSubstitute (ForCycle forName forRange forStatement)
       = ap1On (ForCycle forName (Atom forRange)) forStatement

valueficate
    :: Program (Configuration ByType Pattern Atom)
    -> Program (Configuration   Type Pattern Expression)
valueficate program =
    let referenceInfo = gather program
    in program & programFuncs %~ imap (valueficateFunc referenceInfo)

valueficateFunc
    :: ReferenceInfo
    -> Name
    -> Func (Configuration ByType Pattern Atom)
    -> Func (Configuration   Type Pattern Expression)
valueficateFunc referenceInfo name
    (Func ty (Scope params (Scope vars (Body statement result))))
    = let currentReferenceInfo = referenceInfo M.! name

          params' = params & map (\(name, (_by, ty)) -> (name, ty))

          ty' = foldr1 (TypeApp2 TypePair) (ty : tys)
          result' = foldr1 (\x y -> Call (NameSpecial OpPair) [] [x, y])
                  $ Atom result : map (Atom . Access) results

          (results, tys) = unzip $ do
            (keep, param) <- zip currentReferenceInfo params'
            guard keep
            return param

          statement' = runReader (valueficateSubstitute statement) referenceInfo

      in Func ty' (Scope params' (Scope vars (Body statement' result')))

type ReferenceInfo = Map Name [Bool]

gather :: Program (Configuration ByType pat expr) -> ReferenceInfo
gather = fmap inspect . view programFuncs where
    inspect = map check . view (funcScope . scopeVars)
    check (_name, (by, _ty)) = by == ByReference
