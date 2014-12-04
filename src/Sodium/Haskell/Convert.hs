{-# LANGUAGE FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies #-}
module Sodium.Haskell.Convert (convert, reserved) where

import Data.Traversable
import Data.List (intercalate)
import Control.Applicative
import Data.Singletons.Void
-- S for Src, D for Dest
import qualified Sodium.Nucleus.Vector.Program as S
import qualified Sodium.Haskell.Program as D
import qualified Language.Haskell.Exts        as H
import qualified Language.Haskell.Exts.SrcLoc as H

convert :: S.Program -> H.Module
convert = maybe (error "Sodium.Haskell.Convert") id . conv

class Conv s where

    type Hask s :: *
    conv :: s -> Maybe (Hask s)

instance Conv S.Program where
    type Hask S.Program = H.Module
    conv (S.Program funcs) = do
        funcDefs <- concat <$> traverse conv funcs
        return $ H.Module H.noLoc
            (H.ModuleName "Main")
            (extensions ["LambdaCase", "TupleSections", "MultiWayIf", "ScopedTypeVariables"])
            Nothing
            Nothing
            (map importDecl ["Control.Monad", "Control.Applicative", "Data.Bool"])
            funcDefs
      where extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]
            importDecl s = H.ImportDecl H.noLoc (H.ModuleName s)
                           False False False Nothing Nothing Nothing

-- TODO: the complete list of unqualified names
reserved = keywords ++ words
    " show read readLn getLine return foldl \
    \ map filter undefined foldM main       "
keywords = words
      " _ as case class data default deriving do else hiding \
      \   if import in infix infixl infixr instance let      \
      \   module newtype of qualified then type where        "


transformName :: S.Name -> D.Name
transformName = \case
    S.NameOp op -> convOp op
    S.Name1 ns tag -> let s = intercalate "'" ns in case tag of
        S.IndexTag n -> s ++ "'" ++ show n
        S.ImmutableTag -> s ++ "'" ++ "const"
        S.GlobalTag -> s

instance Conv S.Name where
    type Hask S.Name = D.Name
    conv = return . transformName

instance Conv S.Type where
    type Hask S.Type = H.Type
    conv = \case
        S.TypeInteger -> return $ H.TyCon (D.hsName "Int")
        S.TypeDouble  -> return $ H.TyCon (D.hsName "Double")
        S.TypeBoolean -> return $ H.TyCon (D.hsName "Bool")
        S.TypeChar    -> return $ H.TyCon (D.hsName "Char")
        S.TypeUnit    -> return $ H.TyCon (H.Special H.UnitCon)
        S.TypePair t1 t2  -> (\t1 t2 -> H.TyTuple H.Boxed [t1, t2])
                         <$> conv t1 <*> conv t2
        S.TypeList S.TypeChar -> return $ H.TyCon (D.hsName "String")
        S.TypeList ts -> H.TyList <$> conv ts
        S.TypeFunction t1 t2 -> H.TyFun <$> conv t1 <*> conv t2
        S.TypeTaint t -> H.TyApp (H.TyCon (D.hsName "IO")) <$> conv t


instance Conv S.Expression where

    type Hask S.Expression = H.Exp
    conv (S.Atom expr) = conv expr
    conv (S.Lambda pat act) = H.Lambda H.noLoc <$> traverse conv [pat] <*> conv act
    conv (S.Beta a1 a2) = H.App <$> conv a1 <*> conv a2


instance Conv S.Func where
    type Hask S.Func = [H.Decl]
    conv (S.Func ty (S.NameOp S.OpMain) expression) = do
        hsExpression <- D.matchExpression <$> conv expression
        hsType <- conv ty
        let hsName = H.Ident (convOp S.OpMain)
        let sig = H.TypeSig H.noLoc [hsName] hsType
        return [sig, D.funcDef hsName [] hsExpression]
    conv (S.Func ty name expression) = do
        hsExpression <- D.matchExpression <$> conv expression
        let hsName = H.Ident (transformName name)
        hsType <- conv ty
        let sig = H.TypeSig H.noLoc [hsName] hsType
        return [sig, D.funcDef hsName [] hsExpression]

instance Conv S.Pattern where
    type Hask S.Pattern = H.Pat
    conv S.PUnit = return (H.PTuple H.Boxed [])
    conv S.PWildCard = return H.PWildCard
    conv (S.PAccess name ty) = do
        hsName <- conv name
        hsType <- conv ty
        let annotate pat = H.PatTypeSig H.noLoc pat hsType
        return $ annotate (H.PVar (H.Ident hsName))
    conv (S.PTuple pat1 pat2) = H.PTuple H.Boxed <$> traverse conv [pat1, pat2]


instance Conv S.Atom where

    type Hask S.Atom = H.Exp
    conv (S.Primary lit) = return (convlit lit)
    conv (S.Access name) = D.access <$> conv name

convlit :: S.Literal -> H.Exp
convlit = \case
    S.Lit S.STypeInteger n -> (if n < 0 then H.Paren else id) $ H.Lit (H.Int  n)
    S.Lit S.STypeDouble  x -> (if x < 0 then H.Paren else id) $ H.Lit (H.Frac x)
    S.Lit S.STypeChar    c -> H.Lit $ H.Char c
    S.Lit S.STypeBoolean a -> H.Con $ H.UnQual $ H.Ident (if a then "True" else "False")
    S.Lit S.STypeUnit   () -> H.Con $ H.Special H.UnitCon
    S.Lit (S.STypeList S.STypeChar) cs -> H.Lit $ H.String cs
    S.Lit (S.STypeList t) xs -> H.List $ map (\x -> convlit (S.Lit t x)) xs
    S.Lit (S.STypePair t1 t2) (x1, x2)
         -> H.Tuple H.Boxed [convlit (S.Lit t1 x1), convlit (S.Lit t2 x2)]
    S.Lit (S.STypeFunction _ _) a -> absurd a
    S.Lit (S.STypeTaint _) a -> absurd a

convOp :: S.Operator -> D.Name
convOp = \case
    S.OpNegate   -> "negate"
    S.OpShow     -> "show"
    S.OpIf       -> "bool"
    S.OpFold     -> "foldl"
    S.OpFoldTainted -> "foldM"
    S.OpProduct  -> "product"
    S.OpSum      -> "sum"
    S.OpAnd'     -> "and"
    S.OpOr'      -> "or"
    S.OpAdd      -> "+"
    S.OpSubtract -> "-"
    S.OpMultiply -> "*"
    S.OpDivide   -> "/"
    S.OpDiv      -> "div"
    S.OpMod      -> "mod"
    S.OpMore     -> ">"
    S.OpLess     -> "<"
    S.OpEquals   -> "=="
    S.OpXor      -> "/="
    S.OpAnd      -> "&&"
    S.OpOr       -> "||"
    S.OpNot      -> "not"
    S.OpElem     -> "elem"
    S.OpRange    -> "enumFromTo"
    S.OpId       -> "id"
    S.OpPair     -> ","
    S.OpFst      -> "fst"
    S.OpSnd      -> "snd"
    S.OpPutLn    -> "putStrLn"
    S.OpGetLn    -> "getLine"
    S.OpReadLn   -> "readLn"
    S.OpPrintLn  -> "print"
    S.OpConcat   -> "++"
    S.OpSingleton -> "return"
    S.OpBind      -> ">>="
    S.OpBindIgnore-> ">>"
    S.OpFmapIgnore-> "<$"
    S.OpIgnore    -> "void"
    S.OpTaint     -> "return"
    S.OpIntToDouble -> "fromIntegral"
    S.OpUndefined -> "undefined"
    S.OpMain -> "main"
