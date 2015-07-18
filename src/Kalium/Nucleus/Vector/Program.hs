{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Kalium.Nucleus.Vector.Program where

import Kalium.Prelude
import Kalium.Util

import Control.Monad.Rename

data NameSpecial
    = OpAdd
    | OpSubtract
    | OpMultiply
    | OpDivide
    | OpDiv
    | OpMod
    | OpLess
    | OpMore
    | OpLessEquals
    | OpMoreEquals
    | OpEquals
    | OpNotEquals
    | OpAnd
    | OpOr
    | OpNot
    | OpTrue
    | OpFalse
    | OpRange
    | OpElem
    | OpShow
    | OpNegate
    | OpIf
    | OpFold
    | OpFoldTainted
    | OpFlipMapTaintedIgnore
    | OpMapTaintedIgnore
    | OpProduct
    | OpSum
    | OpAnd'
    | OpOr'
    | OpPrintLn
    | OpReadLn
    | OpPutLn
    | OpPut
    | OpChr
    | OpChrOrd
    | OpGetLn
    | OpGetChar
    | OpId
    | OpUnit
    | OpPair
    | OpFst
    | OpSnd
    | OpSwap
    | OpNil
    | OpCons
    | OpSingleton
    | OpIx
    | OpIxSet
    | OpLength
    | OpTaint
    | OpBind
    | OpBindIgnore
    | OpFmap
    | OpFmapIgnore
    | OpIgnore
    | OpWhen
    | OpConcat
    | OpTake
    | OpReplicate
    | OpRepeat
    | OpIntToDouble
    | OpUndefined
    | OpMain
    | OpTypeInteger
    | OpTypeDouble
    | OpTypeBoolean
    | OpTypeChar
    | OpTypeUnit
    | OpTypeList
    | OpTypePair
    | OpTypeFunction
    | OpTypeTaint
    deriving (Eq, Ord)

data Name = NameSpecial NameSpecial
          | NameGen Integer
    deriving (Eq, Ord)

alias :: (MonadNameGen m) => EndoKleisli' m Name
alias (NameGen m) = NameGen <$> rename m
alias _ = NameGen <$> mkname Nothing

data Type
    = TypeBeta Type Type
    | TypeAccess Name
    deriving (Eq, Ord)

pattern TypeInteger = TypeAccess (NameSpecial OpTypeInteger)
pattern TypeDouble  = TypeAccess (NameSpecial OpTypeDouble)
pattern TypeBoolean = TypeAccess (NameSpecial OpTypeBoolean)
pattern TypeChar    = TypeAccess (NameSpecial OpTypeChar)
pattern TypeUnit    = TypeAccess (NameSpecial OpTypeUnit)
pattern TypeList    = TypeAccess (NameSpecial OpTypeList)
pattern TypePair    = TypeAccess (NameSpecial OpTypePair)
pattern TypeFunction= TypeAccess (NameSpecial OpTypeFunction)
pattern TypeTaint   = TypeAccess (NameSpecial OpTypeTaint)

data Literal
    = LitInteger Integer
    | LitDouble  Rational
    | LitChar    Char
    deriving (Eq)

data Program
    = Program
    { _programFuncs :: Map Name Func
    } deriving (Eq)

data Func
    = Func
    { _funcType :: Type
    , _funcExpression :: Expression
    } deriving (Eq)

data Expression' pext ext
    = Access Name
    | Primary Literal
    | Lambda (Pattern' pext) (Expression' pext ext)
    | Beta (Expression' pext ext) (Expression' pext ext)
    | Ext ext
    deriving (Eq)

type Expression = Expression' Void Void

data Pattern' pext
    = PTuple (Pattern' pext) (Pattern' pext)
    | PAccess Name Type
    | PWildCard
    | PUnit
    | PExt pext
    deriving (Eq)

type Pattern = Pattern' Void

pattern OpAccess op = Access (NameSpecial op)
pattern LitUnit = OpAccess OpUnit

pattern App1 op a        = op `Beta` a
pattern App2 op a1 a2    = op `Beta` a1 `Beta` a2
pattern App3 op a1 a2 a3 = op `Beta` a1 `Beta` a2 `Beta` a3

pattern AppOp1 op a        = App1 (OpAccess op) a
pattern AppOp2 op a1 a2    = App2 (OpAccess op) a1 a2
pattern AppOp3 op a1 a2 a3 = App3 (OpAccess op) a1 a2 a3

pattern Lambda2 p1 p2 a = Lambda p1 (Lambda p2 a)

pattern Into p x a = Beta (Lambda p a) x
pattern Eta p x a = Lambda p (Beta x a)

pattern Ignore a     = AppOp1 OpIgnore a
pattern Taint  a     = AppOp1 OpTaint  a
pattern Bind   a1 a2 = AppOp2 OpBind   a1 a2
pattern Follow p x a = Bind x (Lambda p a)

lambda :: [Pattern' pext] -> Expression' pext ext -> Expression' pext ext
lambda = flip (foldr Lambda)

unlambda :: Expression' pext ext -> ([Pattern' pext], Expression' pext ext)
unlambda = \case
    Lambda p a -> let (ps, b) = unlambda a in (p:ps, b)
    e -> ([], e)

pattern TypeApp1 t t1    = t `TypeBeta` t1
pattern TypeApp2 t t1 t2 = t `TypeBeta` t1 `TypeBeta` t2

tyfun :: [Type] -> Type -> Type
tyfun = flip (foldr (TypeApp2 TypeFunction))

untyfun :: Type -> ([Type], Type)
untyfun = \case
    TypeApp2 TypeFunction tyArg tyRes ->
        let (tyArgs, tyRes') = untyfun tyRes
        in (tyArg:tyArgs, tyRes')
    ty -> ([], ty)

beta :: [Expression' pext ext] -> Expression' pext ext
beta = foldl1 Beta

unbeta :: Expression' pext ext -> [Expression' pext ext]
unbeta = reverse . go where
    go = \case
        Beta a b -> b : go a
        e -> [e]

etaExpand
     :: (MonadNameGen m)
     => [Type] -> EndoKleisli' m (Expression' pext ext)
etaExpand tys e = do
    (unzip -> (exps, pats)) <- forM tys $ \ty -> do
        name <- NameGen <$> mkname Nothing
        return (Access name, PAccess name ty)
    return $ lambda pats $ beta (e:exps)

typeDrivenUnlambda
    :: (MonadNameGen m)
    => Type -> Expression -> m ( ([Pattern],[Type]) , (Expression,Type) )
typeDrivenUnlambda ty a =
  let (tys, ty') = untyfun ty
      ( ps,  a') = unlambda a
      tysLength = length tys
      psLength = length  ps
  in if | tysLength < psLength -> do
            error "lambda-abstraction with non-function type"

        | tysLength > psLength -> do
            let tys' = drop psLength tys
            b <- etaExpand tys' a -- TODO: or is it a'?
            typeDrivenUnlambda ty b

        | otherwise -> return ( (ps,tys) , (a',ty') )

makeLenses ''Func
makeLenses ''Program
