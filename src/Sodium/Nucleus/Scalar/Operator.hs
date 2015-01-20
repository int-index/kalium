module Sodium.Nucleus.Scalar.Operator where

import Sodium.Prelude
import Sodium.Nucleus.Scalar.Program
import qualified Sodium.Nucleus.Vector.Program as Vec

import qualified Data.Map as M

data Operator = Operator
    { tc :: [Type] -> [Type] -> Maybe Type
    , vecname :: Vec.Name
    , vecpure :: Bool
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

operators :: Map Name Operator
operators = M.fromList
    [ OpAdd # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require isNumericType
        , vecname = Vec.NameSpecial Vec.OpAdd
        , vecpure = True
        }

    , OpSubtract # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require isNumericType
        , vecname = Vec.NameSpecial Vec.OpSubtract
        , vecpure = True
        }

    , OpMultiply # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require isNumericType
        , vecname = Vec.NameSpecial Vec.OpMultiply
        , vecpure = True
        }

    , OpDivide # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeDouble)
        , vecname = Vec.NameSpecial Vec.OpDivide
        , vecpure = True
        }

    , OpDiv # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeInteger)
        , vecname = Vec.NameSpecial Vec.OpDiv
        , vecpure = True
        }

    , OpMod # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeInteger)
        , vecname = Vec.NameSpecial Vec.OpMod
        , vecpure = True
        }

    , OpLess # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isOrdType
            return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpLess
        , vecpure = True
        }

    , OpMore # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isOrdType
            return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpMore
        , vecpure = True
        }

    , OpLessEquals # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isOrdType
            return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpLessEquals
        , vecpure = True
        }

    , OpMoreEquals # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isOrdType
            return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpMoreEquals
        , vecpure = True
        }

    , OpEquals # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isEqType
            return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpEquals
        , vecpure = True
        }

    , OpNotEquals # Operator
        { tc = nta $ \args -> do
            listMatch2Same args >>= require isEqType
            return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpNotEquals
        , vecpure = True
        }

    , OpAnd # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeBoolean)
        , vecname = Vec.NameSpecial Vec.OpAnd
        , vecpure = True
        }

    , OpOr # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeBoolean)
        , vecname = Vec.NameSpecial Vec.OpOr
        , vecpure = True
        }

    , OpXor # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require (==TypeBoolean)
        , vecname = Vec.NameSpecial Vec.OpNotEquals
        , vecpure = True
        }

    , OpNot # Operator
        { tc = nta $ \args -> listMatch1 args >>= require (==TypeBoolean)
        , vecname = Vec.NameSpecial Vec.OpNot
        , vecpure = True
        }

    , OpTrue # Operator
        { tc = nta $ \args -> guard (null args) >> return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpTrue
        , vecpure = True
        }

    , OpFalse # Operator
        { tc = nta $ \args -> guard (null args) >> return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpFalse
        , vecpure = True
        }

    , OpRange # Operator
        { tc = nta $ \args -> TypeList <$> listMatch2Same args
        , vecname = Vec.NameSpecial Vec.OpRange
        , vecpure = True
        }

    , OpElem # Operator
        { tc = nta $ \args -> do
            (ty, tys) <- listMatch2 args
            guard (tys == TypeList ty)
            return TypeBoolean
        , vecname = Vec.NameSpecial Vec.OpElem
        , vecpure = True
        }

    , OpShow # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require isShowType
            return (TypeList TypeChar)
        , vecname = Vec.NameSpecial Vec.OpShow
        , vecpure = True
        }

    , OpNegate # Operator
        { tc = nta $ \args -> listMatch1 args >>= require isNumericType
        , vecname = Vec.NameSpecial Vec.OpNegate
        , vecpure = True
        }

    , OpPrintLn # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require isShowType
            return TypeUnit
        , vecname = Vec.NameSpecial Vec.OpPrintLn
        , vecpure = False
        }

    , OpReadLn # Operator
        { tc = \tyArgs args -> do
            [ty] <- return tyArgs
            [  ] <- return args
            return ty
        , vecname = Vec.NameSpecial Vec.OpReadLn
        , vecpure = False
        }

    , OpPutLn # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require (==TypeList TypeChar)
            return TypeUnit
        , vecname = Vec.NameSpecial Vec.OpPutLn
        , vecpure = False
        }

    , OpPut # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require (==TypeList TypeChar)
            return TypeUnit
        , vecname = Vec.NameSpecial Vec.OpPut
        , vecpure = False
        }

    , OpGetLn # Operator
        { tc = nta $ \args -> guard (null args) >> return (TypeList TypeChar)
        , vecname = Vec.NameSpecial Vec.OpGetLn
        , vecpure = False
        }

    , OpUnit # Operator
        { tc = nta $ \args -> guard (null args) >> return TypeUnit
        , vecname = Vec.NameSpecial Vec.OpUnit
        , vecpure = True
        }

    , OpId # Operator
        { tc = nta listMatch1
        , vecname = Vec.NameSpecial Vec.OpId
        , vecpure = True
        }

    , OpPair # Operator
        { tc = nta $ \args -> uncurry TypePair <$> listMatch2 args
        , vecname = Vec.NameSpecial Vec.OpPair
        , vecpure = True
        }

    , OpFst # Operator
        { tc = error "OpFst"
        , vecname = Vec.NameSpecial Vec.OpFst
        , vecpure = True
        }

    , OpSnd # Operator
        { tc = error "OpSnd"
        , vecname = Vec.NameSpecial Vec.OpSnd
        , vecpure = True
        }

    , OpNil # Operator
        { tc = \tyArgs args -> do
            [ty] <- return tyArgs
            [  ] <- return args
            return (TypeList ty)
        , vecname = Vec.NameSpecial Vec.OpNil
        , vecpure = True
        }

    , OpCons # Operator
        { tc = nta $ \args -> do
            (ty, tys) <- listMatch2 args
            guard (tys == TypeList ty)
            return tys
        , vecname = Vec.NameSpecial Vec.OpCons
        , vecpure = True
        }

    , OpSingleton # Operator
        { tc = nta $ \args -> TypeList <$> listMatch1 args
        , vecname = Vec.NameSpecial Vec.OpSingleton
        , vecpure = True
        }

    , OpIx # Operator
        { tc = nta $ \case
            [TypeList ty, TypeInteger] -> return ty
            _ -> mzero
        , vecname = Vec.NameSpecial Vec.OpIx
        , vecpure = True
        }

    , OpIxSet # Operator
        { tc = nta $ \case
            [TypeInteger, ty1, tys@(TypeList ty)]
                | ty1 == ty -> return tys
            _ -> mzero
        , vecname = Vec.NameSpecial Vec.OpIxSet
        , vecpure = True
        }

    , OpLength # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require isListType
            return TypeInteger
        , vecname = Vec.NameSpecial Vec.OpLength
        , vecpure = True
        }

    , OpConcat # Operator
        { tc = nta $ \args -> listMatch2Same args >>= require isListType
        , vecname = Vec.NameSpecial Vec.OpConcat
        , vecpure = True
        }

    , OpIntToDouble # Operator
        { tc = nta $ \args -> do
            listMatch1 args >>= require (==TypeInteger)
            return TypeDouble
        , vecname = Vec.NameSpecial Vec.OpIntToDouble
        , vecpure = True
        }
    ]
  where
    nta m [] = m
    nta _  _ = const Nothing

    (#) :: NameSpecial -> a -> (Name, a)
    (#) (NameSpecial -> name) = (,) name
