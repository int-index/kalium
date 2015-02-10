module Sodium.Nucleus.Scalar.Operator where

import Sodium.Prelude
import Sodium.Nucleus.Scalar.Program
import qualified Sodium.Nucleus.Vector.Program as Vec

import qualified Data.Map as M

data Operator = Operator
    { tc :: [Type] -> [Type] -> Maybe Type
    , vec :: [Vec.Expression] -> Vec.Expression
    }

listMatch1 :: [a] -> Maybe a
listMatch1 = \case
    [a] -> Just a
    _ -> Nothing

listMatch2 :: [a] -> Maybe (a, a)
listMatch2 = \case
    [a1, a2] -> Just (a1, a2)
    _ -> Nothing

pairMatchSame :: Eq a => (a, a) -> Maybe a
pairMatchSame = \case
    (a1, a2) | a1 == a2 -> Just a1
    _ -> Nothing

listMatch2Same :: Eq a => [a] -> Maybe a
listMatch2Same = listMatch2 >=> pairMatchSame

require :: (a -> Bool) -> a -> Maybe a
require p a
    | p a = Just a
    | otherwise = Nothing

isNumericType :: Type -> Bool
isNumericType = (`elem` [TypeInteger, TypeDouble])

isOrdType :: Type -> Bool
isOrdType _ = True -- for now

isEqType :: Type -> Bool
isEqType _ = True -- for now

isShowType :: Type -> Bool
isShowType _ = True -- for now

isListType :: Type -> Bool
isListType = \case
    TypeList _ -> True
    _ -> False

vecApp' :: Vec.Name -> [Vec.Expression] -> Vec.Expression
vecApp' name = Vec.beta . (Vec.Access name :)

vecApp :: Vec.Name -> [Vec.Expression] -> Vec.Expression
vecApp name = Vec.Taint . vecApp' name

operators :: Map Name Operator
operators = M.fromList
    [ OpAdd # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require isNumericType
        , vec = vecApp (Vec.NameSpecial Vec.OpAdd)
        }

    , OpSubtract # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require isNumericType
        , vec = vecApp (Vec.NameSpecial Vec.OpSubtract)
        }

    , OpMultiply # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require isNumericType
        , vec = vecApp (Vec.NameSpecial Vec.OpMultiply)
        }

    , OpDivide # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeDouble)
        , vec = vecApp (Vec.NameSpecial Vec.OpDivide)
        }

    , OpDiv # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeInteger)
        , vec = vecApp (Vec.NameSpecial Vec.OpDiv)
        }

    , OpMod # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeInteger)
        , vec = vecApp (Vec.NameSpecial Vec.OpMod)
        }

    , OpLess # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isOrdType
            return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpLess)
        }

    , OpMore # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isOrdType
            return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpMore)
        }

    , OpLessEquals # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isOrdType
            return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpLessEquals)
        }

    , OpMoreEquals # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isOrdType
            return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpMoreEquals)
        }

    , OpEquals # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isEqType
            return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpEquals)
        }

    , OpNotEquals # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isEqType
            return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpNotEquals)
        }

    , OpAnd # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeBoolean)
        , vec = vecApp (Vec.NameSpecial Vec.OpAnd)
        }

    , OpOr # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeBoolean)
        , vec = vecApp (Vec.NameSpecial Vec.OpOr)
        }

    , OpXor # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeBoolean)
        , vec = vecApp (Vec.NameSpecial Vec.OpNotEquals)
        }

    , OpNot # Operator
        { tc = nta $ \args -> listMatch1 args >>= require (==TypeBoolean)
        , vec = vecApp (Vec.NameSpecial Vec.OpNot)
        }

    , OpTrue # Operator
        { tc = nta $ \args -> guard (null args) >> return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpTrue)
        }

    , OpFalse # Operator
        { tc = nta $ \args -> guard (null args) >> return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpFalse)
        }

    , OpRange # Operator
        { tc = nta $ \args -> TypeList <$> listMatch2Same args
        , vec = vecApp (Vec.NameSpecial Vec.OpRange)
        }

    , OpElem # Operator
        { tc = nta $ \args -> do
            (ty, tys) <- listMatch2 args
            guard (tys == TypeList ty)
            return TypeBoolean
        , vec = vecApp (Vec.NameSpecial Vec.OpElem)
        }

    , OpShow # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require isShowType
            return (TypeList TypeChar)
        , vec = vecApp (Vec.NameSpecial Vec.OpShow)
        }

    , OpNegate # Operator
        { tc = nta $ \args -> listMatch1 args >>= require isNumericType
        , vec = vecApp (Vec.NameSpecial Vec.OpNegate)
        }

    , OpPrintLn # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require isShowType
            return TypeUnit
        , vec = vecApp' (Vec.NameSpecial Vec.OpPrintLn)
        }

    , OpReadLn # Operator
        { tc = \tyArgs args -> do
            [ty] <- return tyArgs
            [  ] <- return args
            return ty
        , vec = vecApp' (Vec.NameSpecial Vec.OpReadLn)
        }

    , OpPutLn # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require (==TypeList TypeChar)
            return TypeUnit
        , vec = vecApp' (Vec.NameSpecial Vec.OpPutLn)
        }

    , OpPut # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require (==TypeList TypeChar)
            return TypeUnit
        , vec = vecApp' (Vec.NameSpecial Vec.OpPut)
        }

    , OpGetLn # Operator
        { tc = nta $ \args -> guard (null args) >> return (TypeList TypeChar)
        , vec = vecApp' (Vec.NameSpecial Vec.OpGetLn)
        }

    , OpUnit # Operator
        { tc = nta $ \args -> guard (null args) >> return TypeUnit
        , vec = vecApp (Vec.NameSpecial Vec.OpUnit)
        }

    , OpId # Operator
        { tc = nta listMatch1
        , vec = vecApp (Vec.NameSpecial Vec.OpId)
        }

    , OpPair # Operator
        { tc = nta $ \args -> uncurry TypePair <$> listMatch2 args
        , vec = vecApp (Vec.NameSpecial Vec.OpPair)
        }

    , OpNil # Operator
        { tc = \tyArgs args -> do
            [ty] <- return tyArgs
            [  ] <- return args
            return (TypeList ty)
        , vec = vecApp (Vec.NameSpecial Vec.OpNil)
        }

    , OpCons # Operator
        { tc = nta $ \args -> do
            (ty, tys) <- listMatch2 args
            guard (tys == TypeList ty)
            return tys
        , vec = vecApp (Vec.NameSpecial Vec.OpCons)
        }

    , OpSingleton # Operator
        { tc = nta $ \args -> TypeList <$> listMatch1 args
        , vec = vecApp (Vec.NameSpecial Vec.OpSingleton)
        }

    , OpIx # Operator
        { tc = nta $ \case
            [TypeList ty, TypeInteger] -> return ty
            _ -> mzero
        , vec = vecApp (Vec.NameSpecial Vec.OpIx)
        }

    , OpIxSet # Operator
        { tc = nta $ \case
            [TypeInteger, ty1, tys@(TypeList ty)]
                | ty1 == ty -> return tys
            _ -> mzero
        , vec = vecApp (Vec.NameSpecial Vec.OpIxSet)
        }

    , OpLength # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require isListType
            return TypeInteger
        , vec = vecApp (Vec.NameSpecial Vec.OpLength)
        }

    , OpSetLength # Operator
        { tc = error "tc: OpSetLength"
        , vec = \case
            [listExpr, lengthExpr] ->
                let vecUndefined  = Vec.OpAccess Vec.OpUndefined
                    vecUndefineds = Vec.AppOp1 Vec.OpRepeat vecUndefined
                    vecPrepend = Vec.AppOp2 Vec.OpConcat listExpr
                    vecList = Vec.AppOp2 Vec.OpTake lengthExpr
                        (vecPrepend vecUndefineds)
                in Vec.Taint vecList
            _ -> error "vec: OpSetLength"
        }

    , OpConcat # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require isListType
        , vec = vecApp (Vec.NameSpecial Vec.OpConcat)
        }

    , OpIntToDouble # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require (==TypeInteger)
            return TypeDouble
        , vec = vecApp (Vec.NameSpecial Vec.OpIntToDouble)
        }
    ]
  where
    nta m [] = m
    nta _  _ = const Nothing

    (#) :: NameSpecial -> a -> (Name, a)
    (#) (NameSpecial -> name) = (,) name
