module Sodium.Haskell.Convert (convert) where

import qualified Data.Map as M
import qualified Sodium.Nucleus.Vector.Program as S
import qualified Language.Haskell.Exts        as H
import qualified Language.Haskell.Exts.SrcLoc as H

convert :: S.Program -> H.Module
convert = convProgram

convProgram :: S.Program -> H.Module
convProgram (S.Program funcs _) =
    H.Module H.noLoc
        (H.main_mod)
        (extensions ["LambdaCase", "TupleSections", "MultiWayIf", "ScopedTypeVariables"])
        Nothing
        Nothing
        (map importDecl ["Control.Monad", "Control.Applicative", "Data.Bool"])
        (M.toList funcs >>= uncurry convFunc)
  where extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]
        importDecl s = H.ImportDecl H.noLoc (H.ModuleName s)
                       False False False Nothing Nothing Nothing

nameGen :: Integer -> H.Name
nameGen n = H.Ident ("_" ++ show n)

convType :: S.Type -> H.Type
convType = \case
    S.TypeInteger -> H.TyCon (H.UnQual (H.Ident "Int"))
    S.TypeDouble  -> H.TyCon (H.UnQual (H.Ident "Double"))
    S.TypeBoolean -> H.TyCon (H.UnQual (H.Ident "Bool"))
    S.TypeChar    -> H.TyCon (H.UnQual (H.Ident "Char"))
    S.TypeUnit    -> H.TyCon (H.Special H.UnitCon)
    S.TypePair t1 t2  -> H.TyTuple H.Boxed [convType t1, convType t2]
    S.TypeList S.TypeChar -> H.TyCon (H.UnQual (H.Ident "String"))
    S.TypeList ts -> H.TyList (convType ts)
    S.TypeFunction t1 t2 -> H.TyFun (convType t1) (convType t2)
    S.TypeTaint t -> H.TyApp (H.TyCon (H.UnQual (H.Ident "IO"))) (convType t)


convExpression :: S.Expression -> H.Exp
convExpression = \case
    S.Lambda pat act -> H.Lambda H.noLoc [convPattern pat] (convExpression act)
    S.Beta a1 a2 -> H.App (convExpression a1) (convExpression a2)
    S.Primary lit -> convLit lit
    S.Access name -> case name of
        S.NameSpecial op -> convOp op
        S.NameGen n -> H.Var (H.UnQual (nameGen n))

convFunc :: S.Name -> S.Func -> [H.Decl]
convFunc name (S.Func ty expression) =
    let hsName = case name of
          S.NameSpecial S.OpMain -> H.main_name
          S.NameGen n -> nameGen n
          _ -> error "convFunc: incorrect name"
        sig = H.TypeSig H.noLoc [hsName] (convType ty)
        hsRhs = H.UnGuardedRhs (convExpression expression)
        funBind = H.FunBind [H.Match H.noLoc hsName [] Nothing hsRhs (H.BDecls [])]
    in [sig, funBind]

convPattern :: S.Pattern -> H.Pat
convPattern = \case
    S.PUnit -> H.PTuple H.Boxed []
    S.PWildCard -> H.PWildCard
    S.PAccess name ty ->
        let hsName = case name of
              S.NameGen n -> nameGen n
              _ -> error "convPattern: incorrect name"
            annotate pat = H.PatTypeSig H.noLoc pat (convType ty)
        in annotate (H.PVar hsName)
    S.PTuple pat1 pat2 -> H.PTuple H.Boxed [convPattern pat1, convPattern pat2]

convLit :: S.Literal -> H.Exp
convLit = \case
    S.LitInteger n -> (if n < 0 then H.Paren else id) $ H.Lit (H.Int  n)
    S.LitDouble  x -> (if x < 0 then H.Paren else id) $ H.Lit (H.Frac x)
    S.LitChar    c -> H.Lit $ H.Char c

convOp :: S.NameSpecial -> H.Exp
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
    S.OpMoreEquals -> H.Var (H.UnQual (H.Symbol ">="))
    S.OpLessEquals -> H.Var (H.UnQual (H.Symbol "<="))
    S.OpEquals   -> H.Var (H.UnQual (H.Symbol "=="))
    S.OpNotEquals-> H.Var (H.UnQual (H.Symbol "/="))
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
    S.OpPut      -> H.Var (H.UnQual (H.Ident "putStr"))
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
