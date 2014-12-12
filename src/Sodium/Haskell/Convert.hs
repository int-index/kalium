{-# LANGUAGE FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies #-}
module Sodium.Haskell.Convert (convert) where

import Data.Traversable
import Control.Applicative
-- S for Src, D for Dest
import qualified Sodium.Nucleus.Vector.Program as S
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
            (H.main_mod)
            (extensions ["LambdaCase", "TupleSections", "MultiWayIf", "ScopedTypeVariables"])
            Nothing
            Nothing
            (map importDecl ["Control.Monad", "Control.Applicative", "Data.Bool"])
            funcDefs
      where extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]
            importDecl s = H.ImportDecl H.noLoc (H.ModuleName s)
                           False False False Nothing Nothing Nothing

nameGen :: Integer -> H.Name
nameGen n = H.Ident ("_" ++ show n)

instance Conv S.Type where
    type Hask S.Type = H.Type
    conv = \case
        S.TypeInteger -> return $ H.TyCon (H.UnQual (H.Ident "Int"))
        S.TypeDouble  -> return $ H.TyCon (H.UnQual (H.Ident "Double"))
        S.TypeBoolean -> return $ H.TyCon (H.UnQual (H.Ident "Bool"))
        S.TypeChar    -> return $ H.TyCon (H.UnQual (H.Ident "Char"))
        S.TypeUnit    -> return $ H.TyCon (H.Special H.UnitCon)
        S.TypePair t1 t2  -> (\t1 t2 -> H.TyTuple H.Boxed [t1, t2])
                         <$> conv t1 <*> conv t2
        S.TypeList S.TypeChar -> return $ H.TyCon (H.UnQual (H.Ident "String"))
        S.TypeList ts -> H.TyList <$> conv ts
        S.TypeFunction t1 t2 -> H.TyFun <$> conv t1 <*> conv t2
        S.TypeTaint t -> H.TyApp (H.TyCon (H.UnQual (H.Ident "IO"))) <$> conv t


instance Conv S.Expression where

    type Hask S.Expression = H.Exp

    conv (S.Lambda pat act) = H.Lambda H.noLoc <$> traverse conv [pat] <*> conv act
    conv (S.Beta a1 a2) = H.App <$> conv a1 <*> conv a2
    conv (S.Primary lit) = return (convLit lit)
    conv (S.Access name) = return $ case name of
        S.NameOp op -> convOp op
        S.NameGen n -> H.Var (H.UnQual (nameGen n))


instance Conv S.Func where
    type Hask S.Func = [H.Decl]
    conv (S.Func ty name expression) = do
        hsExpression <- conv expression
        hsName <- case name of
            S.NameOp S.OpMain -> return H.main_name
            S.NameGen n -> return (nameGen n)
            _ -> Nothing
        hsType <- conv ty
        let sig = H.TypeSig H.noLoc [hsName] hsType
        return [sig, H.FunBind [H.Match H.noLoc hsName [] Nothing (H.UnGuardedRhs hsExpression) (H.BDecls [])]]

instance Conv S.Pattern where
    type Hask S.Pattern = H.Pat
    conv S.PUnit = return (H.PTuple H.Boxed [])
    conv S.PWildCard = return H.PWildCard
    conv (S.PAccess name ty) = do
        hsName <- case name of
            S.NameGen n -> return (nameGen n)
            _ -> Nothing
        hsType <- conv ty
        let annotate pat = H.PatTypeSig H.noLoc pat hsType
        return $ annotate (H.PVar hsName)
    conv (S.PTuple pat1 pat2) = H.PTuple H.Boxed <$> traverse conv [pat1, pat2]

convLit :: S.Literal -> H.Exp
convLit = \case
    S.LitInteger n -> (if n < 0 then H.Paren else id) $ H.Lit (H.Int  n)
    S.LitDouble  x -> (if x < 0 then H.Paren else id) $ H.Lit (H.Frac x)
    S.LitChar    c -> H.Lit $ H.Char c

convOp :: S.Operator -> H.Exp
convOp = \case
    S.OpSingleton -> H.RightSection (H.QConOp H.list_cons_name) (convOp S.OpNil)
    S.OpPair     -> H.tuple_con H.Boxed 1
    S.OpCons     -> H.Con H.list_cons_name
    S.OpUnit     -> H.unit_con
    S.OpNil      -> H.List []
    S.OpNegate   -> H.Var (H.UnQual (H.Ident "negate"))
    S.OpShow     -> H.Var (H.UnQual (H.Ident "show"))
    S.OpIf       -> H.Var (H.UnQual (H.Ident "bool"))
    S.OpFold     -> H.Var (H.UnQual (H.Ident "foldl"))
    S.OpFoldTainted -> H.Var (H.UnQual (H.Ident "foldM"))
    S.OpProduct  -> H.Var (H.UnQual (H.Ident "product"))
    S.OpSum      -> H.Var (H.UnQual (H.Ident "sum"))
    S.OpAnd'     -> H.Var (H.UnQual (H.Ident "and"))
    S.OpOr'      -> H.Var (H.UnQual (H.Ident "or"))
    S.OpAdd      -> H.Var (H.UnQual (H.Symbol "+"))
    S.OpSubtract -> H.Var (H.UnQual (H.Symbol "-"))
    S.OpMultiply -> H.Var (H.UnQual (H.Symbol "*"))
    S.OpDivide   -> H.Var (H.UnQual (H.Symbol "/"))
    S.OpDiv      -> H.Var (H.UnQual (H.Ident "div"))
    S.OpMod      -> H.Var (H.UnQual (H.Ident "mod"))
    S.OpMore     -> H.Var (H.UnQual (H.Symbol ">"))
    S.OpLess     -> H.Var (H.UnQual (H.Symbol "<"))
    S.OpEquals   -> H.Var (H.UnQual (H.Symbol "=="))
    S.OpXor      -> H.Var (H.UnQual (H.Symbol "/="))
    S.OpTrue     -> H.Con (H.UnQual (H.Ident "True"))
    S.OpFalse    -> H.Con (H.UnQual (H.Ident "False"))
    S.OpAnd      -> H.Var (H.UnQual (H.Symbol "&&"))
    S.OpOr       -> H.Var (H.UnQual (H.Symbol "||"))
    S.OpNot      -> H.Var (H.UnQual (H.Ident "not"))
    S.OpElem     -> H.Var (H.UnQual (H.Ident "elem"))
    S.OpRange    -> H.Var (H.UnQual (H.Ident "enumFromTo"))
    S.OpId       -> H.Var (H.UnQual (H.Ident "id"))
    S.OpFst      -> H.Var (H.UnQual (H.Ident "fst"))
    S.OpSnd      -> H.Var (H.UnQual (H.Ident "snd"))
    S.OpSwap     -> H.Var (H.UnQual (H.Ident "swap"))
    S.OpPutLn    -> H.Var (H.UnQual (H.Ident "putStrLn"))
    S.OpGetLn    -> H.Var (H.UnQual (H.Ident "getLine"))
    S.OpReadLn   -> H.Var (H.UnQual (H.Ident "readLn"))
    S.OpPrintLn  -> H.Var (H.UnQual (H.Ident "print"))
    S.OpConcat   -> H.Var (H.UnQual (H.Symbol "++"))
    S.OpBind      -> H.Var (H.UnQual (H.Symbol ">>="))
    S.OpBindIgnore-> H.Var (H.UnQual (H.Symbol ">>"))
    S.OpFmapIgnore-> H.Var (H.UnQual (H.Symbol "<$"))
    S.OpIgnore    -> H.Var (H.UnQual (H.Ident "void"))
    S.OpTaint     -> H.Var (H.UnQual (H.Ident "return"))
    S.OpIntToDouble -> H.Var (H.UnQual (H.Ident "fromIntegral"))
    S.OpUndefined -> H.Var (H.UnQual (H.Ident "undefined"))
    S.OpMain -> H.Var (H.UnQual (H.main_name))
