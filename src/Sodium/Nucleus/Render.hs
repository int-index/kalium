module Sodium.Nucleus.Render where

import Data.List
import qualified Data.Map as M
import Control.Lens hiding (Index, Fold)

import Sodium.Nucleus.Program
import Sodium.Nucleus.Program.Vector

class Render a where
    render :: a -> String

tick :: Char
tick = '`'

indent :: String -> String
indent = concat . intersperse "\n" . map ("  "++) . lines

parens :: String -> String
parens s = "(" ++ s ++ ")"

commas :: [String] -> String
commas = parens . intercalate ", "

instance Render Name where
    render = \case
        Name cs   -> cs
        NameMain  -> tick : "main"
        NameGen n -> tick : show n
        NameUnique name -> tick : "uniq" ++ tick : render name

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
        OpReadLn ty -> "readln-" ++ render ty
        OpId -> "id"
        OpName name -> render name

instance Render FuncSig where
    render funcSig = unwords
        [ render (funcSig ^. funcName)
        , let r (name, (by, ty)) = render name ++ ": " ++ rby by (render ty)
              rby ByValue     = id
              rby ByReference = ('$':)
              cat [] = "_"
              cat xs = commas xs
          in cat $ map r (funcSig ^. funcParams)
        , "->"
        , render (funcSig ^. funcRetType)
        ]

instance Render Literal where
    render = \case
        LitInteger x -> show x
        LitDouble  x -> show x
        LitBoolean x -> show x
        LitString  x -> show x
        LitUnit -> "()"

instance Render Type where
    render = \case
        TypeInteger -> "Integer"
        TypeDouble  -> "Double"
        TypeBoolean -> "Boolean"
        TypeString  -> "String"
        TypeUnit    -> "()"

instance Render Program where
    render = unlines
           . map render
           . view programFuncs

instance Render Func where
    render func = unlines
        [ render (func ^. funcSig)
        , indent $ render (func ^. funcBody)
        ]

instance Render Body where
    render body = unlines $
        [ "$$"
        , let vars = map
                (\(name, ty) -> render name ++ ": " ++ render ty)
                (body ^. bodyVars . to M.toList)
          in indent (unlines vars)
        , ".."
        ] ++ map render (body ^. bodyBinds) ++
        [ unwords ["=>", render (body ^. bodyResult)]
        ]

instance Render Bind where
    render bind = render (bind ^. bindPattern)
                ++ " =\n"
                ++ indent (render (bind ^. bindStatement))

instance Render Statement where
    render = \case
        Assign expr -> render expr
        Execute op args -> render op ++ commas (map render args)
        BodyStatement body -> render body
        MultiIfStatement multiIfBranch -> render multiIfBranch
        ForStatement forCycle -> render forCycle

instance Render Pattern where
    render = \case
        PTuple pats -> commas (map render pats)
        PAccess name index -> render name ++ render index
        PWildCard -> "_"

instance Render Index where
    render = \case
        Index i -> "-" ++ show i
        Immutable -> "#"
        Uninitialized -> "?"

instance Render Expression where
    render = \case
        Tuple exprs -> commas (map render exprs)
        Access name index -> render name ++ render index
        Primary lit -> render lit
        Call op args -> render op ++ commas (map render args)
        expr -> show expr

instance Render MultiIfBranch where
    render multiIfBranch =
        let rLeaf (cond, statement)
                = "| " ++ render cond ++ "\n"
                       ++ indent (render statement)
            r = map rLeaf
                $ (multiIfBranch ^. multiIfLeafs)
                ++ [(Primary (LitBoolean True), multiIfBranch ^. multiIfElse)]
        in unlines r

instance Render ForCycle where
    render _ = "LOOP"