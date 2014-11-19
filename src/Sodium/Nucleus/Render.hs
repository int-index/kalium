{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Sodium.Nucleus.Render where

import Prelude   hiding (unlines)
import Data.List hiding (unlines)
import qualified Data.Map as M
import Control.Lens hiding (Index, Fold)

import Sodium.Nucleus.Program
import Sodium.Nucleus.Program.Vector

unlines = intercalate "\n"

class Render a where
    render :: a -> String

tick :: Char
tick = '`'

indent :: String -> String
indent = unlines . map ("  "++) . filter (not.null) . lines

parens :: String -> String
parens s = "(" ++ s ++ ")"

commas :: [String] -> String
commas = parens . intercalate ", "

instance Render Name where
    render = \case
        NameSpace namespace name -> namespace ++ "'" ++ render name
        Name name -> name
        NameMain  -> tick : "main"
        NameOp op -> render op

instance Render Operator where
    render = \case
        OpAdd -> "add"
        OpSubtract -> "subtract"
        OpMultiply -> "multiply"
        OpDivide -> "divide"
        OpDiv -> "div"
        OpMod -> "mod"
        OpLess -> "less"
        OpMore -> "more"
        OpEquals -> "equals"
        OpAnd -> "and"
        OpOr  -> "or"
        OpNot -> "not"
        OpXor -> "xor"
        OpRange -> "range"
        OpElem -> "elem"
        OpShow -> "show"
        OpNegate -> "negate"
        OpProduct -> "product"
        OpSum -> "sum"
        OpAnd' -> "and'"
        OpOr' -> "or'"
        OpPrintLn -> "println"
        OpGetLn -> "getln"
        OpReadLn ty -> "readln-" ++ render ty
        OpId -> "id"
        OpPair -> "pair"
        OpFst -> "fst"
        OpSnd -> "snd"
        OpSingleton -> "single"
        OpIntToDouble -> "cast-" ++ render TypeInteger ++ "-" ++ render TypeDouble

instance Render FuncSig where
    render funcSig = unwords
        [ render (funcSig ^. funcName)
        , "->"
        , render (funcSig ^. funcRetType)
        ]

instance Render Literal where
    render (Lit STypeInteger a) = show a
    render (Lit STypeDouble  a) = show a
    render (Lit STypeBoolean a) = show a
    render (Lit STypeChar    a) = show a
    render (Lit STypeUnit    a) = show a
    render (Lit (STypeList STypeChar) a) = show a
    render _ = "LITERAL"

instance Render Type where
    render = \case
        TypeInteger -> "Integer"
        TypeDouble  -> "Double"
        TypeBoolean -> "Boolean"
        TypeChar    -> "Char"
        TypeList t  -> "List " ++ parens (render t)
        TypeUnit    -> parens ""
        TypePair t1 t2 -> parens (commas [render t1, render t2])

instance Render Program where
    render = unlines
           . map render
           . view programFuncs

instance Render Func where
    render func = unlines
        [ render (func ^. funcSig)
        , indent $ render (func ^. funcStatement)
        ]

instance Render Body where
    render body = unlines $
        ((\(name, ty) -> "$ " ++ render name ++ ": " ++ render ty ++ "\n")
        `map` (body ^. bodyVars . to M.toList))
        ++ map render (body ^. bodyBinds) ++
        [ unwords ["=>", render (body ^. bodyResult)]
        ]

instance Render (Bind Statement) where
    render bind = render (bind ^. bindPattern)
                ++ " =\n"
                ++ indent (render (bind ^. bindStatement))

instance Render Statement where
    render = \case
        Assign expr -> render expr
        Execute op args -> render op ++ commas (map render args)
        BodyStatement body -> render body
        MultiIfStatement multiIf -> render multiIf
        ForStatement forCycle -> render forCycle

instance Render Pattern where
    render = \case
        PUnit -> "()"
        PTuple pat1 pat2 -> commas (map render [pat1, pat2])
        PAccess name index -> render name ++ render index
        PWildCard -> "_"

instance Render Index where
    render = \case
        Index i -> "-" ++ show i
        Immutable -> "#"
        Uninitialized -> "?"

instance Render Expression where
    render = \case
        Access name index -> render name ++ render index
        Primary lit -> render lit
        Call op args -> render op ++ commas (map render args)
        MultiIfExpression multiIf -> render multiIf
        _ -> "EXPR"

instance Render (MultiIf Statement) where
    render multiIf =
        let rLeaf (cond, statement)
                = "| " ++ render cond ++ "\n"
                       ++ indent (render statement)
        in unlines $ map rLeaf $ multiIf ^. multiIfLeafs

instance Render (MultiIf Expression) where
    render multiIf =
        let rLeaf (cond, expression)
                = "| " ++ render cond ++ ": " ++ render expression ++ ";"
        in "(" ++ unwords (map rLeaf $ multiIf ^. multiIfLeafs) ++ ")"

instance Render (ForCycle a) where
    render _ = "LOOP"
