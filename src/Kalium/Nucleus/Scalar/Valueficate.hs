{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Kalium.Nucleus.Scalar.Valueficate where

import Kalium.Prelude

import qualified Data.Map as M

import Kalium.Nucleus.Scalar.Program

class ValueficateSubstitute a where
    valueficateSubstitute
        :: ReferenceInfo
        -> a (Configuration param0 Pattern Atom)
        -> a (Configuration param1 Pattern Expression)

instance ValueficateSubstitute Exec where
    valueficateSubstitute referenceInfo (Exec ret op tyArgs args) =
        let ret' = foldr1 PTuple (ret:rets)
            rets = case M.lookup op referenceInfo of
                Nothing -> []
                Just currentReferenceInfo -> do
                    (keep, arg) <- zip currentReferenceInfo args
                    guard keep
                    return $ case arg of
                        Access name -> PAccess name
                        Primary _ -> error "non-variables by-ref"
        in Exec ret' op tyArgs (map Atom args)

instance ValueficateSubstitute Statement where
    valueficateSubstitute referenceInfo = \case
        Follow a1 a2 -> Follow (go a1) (go a2)
        ScopeStatement a -> ScopeStatement (go a)
        IfStatement  a -> IfStatement  (go a)
        ForStatement a -> ForStatement (go a)
        Execute      a -> Execute      (go a)
        Pass -> Pass
        where go = valueficateSubstitute referenceInfo

instance   ValueficateSubstitute obj
        => ValueficateSubstitute (Scope vars obj) where
    valueficateSubstitute referenceInfo
       = scopeElem %~ valueficateSubstitute referenceInfo

instance ValueficateSubstitute If where
    valueficateSubstitute referenceInfo
        (If       ifCond      ifThen      ifElse)
       = If (Atom ifCond) (go ifThen) (go ifElse)
       where go = valueficateSubstitute referenceInfo

instance ValueficateSubstitute ForCycle where
    valueficateSubstitute referenceInfo
        (ForCycle forName       forRange      forStatement)
       = ForCycle forName (Atom forRange) (go forStatement)
       where go = valueficateSubstitute referenceInfo

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

          statement' = valueficateSubstitute referenceInfo statement

      in Func ty' (Scope params' (Scope vars (Body statement' result')))

type ReferenceInfo = Map Name [Bool]

gather :: Program (Configuration ByType pat expr) -> ReferenceInfo
gather = fmap inspect . view programFuncs where
    inspect = map check . view (funcScope . scopeVars)
    check (_name, (by, _ty)) = by == ByReference
