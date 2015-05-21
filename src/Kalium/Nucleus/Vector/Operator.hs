module Kalium.Nucleus.Vector.Operator where

import Kalium.Prelude
import Kalium.Nucleus.Vector.Program
import Kalium.Haskell.Common
import qualified Language.Haskell.Exts as H

import qualified Data.Map as M

data Operator = Operator
    { hs :: H.Exp
    }

operators :: Map Name Operator
operators = M.fromList
    [ OpSingleton # Operator
        { hs = H.RightSection
            (H.QConOp H.list_cons_name)
            (hs $ operators M.! NameSpecial OpNil)
        }
    , OpPair # Operator
        { hs = H.tuple_con H.Boxed 1
        }
    , OpCons # Operator
        { hs = H.Con H.list_cons_name
        }
    , OpUnit # Operator
        { hs = H.unit_con
        }
    , OpNil # Operator
        { hs = H.List []
        }
    , OpIx # Operator
        { hs = H.Var (HsSymbol "Prelude" "!!")
        }
    , OpIxSet # Operator
        { hs = (H.Var (HsSymbol "Prelude" "."))
            `H.App` H.Var (HsIdent "Control.Lens" "set")
            `H.App` H.Var (HsIdent "Control.Lens" "ix")
        }
    , OpLength # Operator
        { hs = H.Var (HsIdent "Prelude" "length")
        }
    , OpNegate # Operator
        { hs = H.Var (HsIdent "Prelude" "negate")
        }
    , OpShow # Operator
        { hs = H.Var (HsIdent "Prelude" "show")
        }
    , OpIf # Operator
        { hs = H.Var (HsIdent "Data.Bool" "bool")
        }
    , OpFold # Operator
        { hs = H.Var (HsIdent "Prelude" "foldl")
        }
    , OpFoldTainted # Operator
        { hs = H.Var (HsIdent "Control.Monad" "foldM")
        }
    , OpFlipMapTaintedIgnore # Operator
        { hs = H.Var (HsIdent "Data.Foldable" "for_")
        }
    , OpMapTaintedIgnore # Operator
        { hs = H.Var (HsIdent "Data.Foldable" "traverse_")
        }
    , OpProduct # Operator
        { hs = H.Var (HsIdent "Prelude" "product")
        }
    , OpSum # Operator
        { hs = H.Var (HsIdent "Prelude" "sum")
        }
    , OpAnd' # Operator
        { hs = H.Var (HsIdent "Prelude" "and")
        }
    , OpOr' # Operator
        { hs = H.Var (HsIdent "Prelude" "or")
        }
    , OpAdd # Operator
        { hs = H.Var (HsSymbol "Prelude" "+")
        }
    , OpSubtract # Operator
        { hs = H.Var (HsSymbol "Prelude" "-")
        }
    , OpMultiply # Operator
        { hs = H.Var (HsSymbol "Prelude" "*")
        }
    , OpDivide # Operator
        { hs = H.Var (HsSymbol "Prelude" "/")
        }
    , OpDiv # Operator
        { hs = H.Var (HsIdent "Prelude" "div")
        }
    , OpMod # Operator
        { hs = H.Var (HsIdent "Prelude" "mod")
        }
    , OpMore # Operator
        { hs = H.Var (HsSymbol "Prelude" ">")
        }
    , OpLess # Operator
        { hs = H.Var (HsSymbol "Prelude" "<")
        }
    , OpMoreEquals # Operator
        { hs = H.Var (HsSymbol "Prelude" ">=")
        }
    , OpLessEquals # Operator
        { hs = H.Var (HsSymbol "Prelude" "<=")
        }
    , OpEquals # Operator
        { hs = H.Var (HsSymbol "Prelude" "==")
        }
    , OpNotEquals # Operator
        { hs = H.Var (HsSymbol "Prelude" "/=")
        }
    , OpTrue # Operator
        { hs = H.Con (HsIdent "Prelude" "True")
        }
    , OpFalse # Operator
        { hs = H.Con (HsIdent "Prelude" "False")
        }
    , OpAnd # Operator
        { hs = H.Var (HsSymbol "Prelude" "&&")
        }
    , OpOr # Operator
        { hs = H.Var (HsSymbol "Prelude" "||")
        }
    , OpNot # Operator
        { hs = H.Var (HsIdent "Prelude" "not")
        }
    , OpElem # Operator
        { hs = H.Var (HsIdent "Prelude" "elem")
        }
    , OpRange # Operator
        { hs = H.Var (HsIdent "Prelude" "enumFromTo")
        }
    , OpId # Operator
        { hs = H.Var (HsIdent "Prelude" "id")
        }
    , OpFst # Operator
        { hs = H.Var (HsIdent "Prelude" "fst")
        }
    , OpSnd # Operator
        { hs = H.Var (HsIdent "Prelude" "snd")
        }
    , OpSwap # Operator
        { hs = H.Var (HsIdent "Data.Tuple" "swap")
        }
    , OpPutLn # Operator
        { hs = H.Var (HsIdent "Prelude" "putStrLn")
        }
    , OpPut # Operator
        { hs = H.Var (HsIdent "Prelude" "putStr")
        }
    , OpChr # Operator
        { hs = H.Var (HsIdent "Data.Char" "chr")
        }
    , OpChrOrd # Operator
        { hs = H.Var (HsIdent "Data.Char" "ord")
        }
    , OpGetLn # Operator
        { hs = H.Var (HsIdent "Prelude" "getLine")
        }
    , OpGetChar # Operator
        { hs = H.Var (HsIdent "Prelude" "getChar")
        }
    , OpReadLn # Operator
        { hs = H.Var (HsIdent "Prelude" "readLn")
        }
    , OpPrintLn # Operator
        { hs = H.Var (HsIdent "Prelude" "print")
        }
    , OpConcat # Operator
        { hs = H.Var (HsSymbol "Prelude" "++")
        }
    , OpTake # Operator
        { hs = H.Var (HsIdent "Prelude" "take")
        }
    , OpRepeat # Operator
        { hs = H.Var (HsIdent "Prelude" "repeat")
        }
    , OpBind # Operator
        { hs = H.Var (HsSymbol "Prelude" ">>=")
        }
    , OpBindIgnore # Operator
        { hs = H.Var (HsSymbol "Prelude" ">>")
        }
    , OpFmap # Operator
        { hs = H.Var (HsSymbol "Control.Applicative" "<$>")
        }
    , OpFmapIgnore # Operator
        { hs = H.Var (HsSymbol "Control.Applicative" "<$")
        }
    , OpIgnore # Operator
        { hs = H.Var (HsIdent "Control.Monad" "void")
        }
    , OpWhen # Operator
        { hs = H.Var (HsIdent "Control.Monad" "when")
        }
    , OpTaint # Operator
        { hs = H.Var (HsIdent "Prelude" "return")
        }
    , OpIntToDouble # Operator
        { hs = H.Var (HsIdent "Prelude" "fromIntegral")
        }
    , OpUndefined # Operator
        { hs = H.Var (HsIdent "Prelude" "undefined")
        }
    , OpMain # Operator
        { hs = H.Var (H.UnQual (H.main_name))
        }
    ]
  where
    (#) :: NameSpecial -> a -> (Name, a)
    (#) (NameSpecial -> name) = (,) name
