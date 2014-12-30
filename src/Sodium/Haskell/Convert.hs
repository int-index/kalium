{-# LANGUAGE FlexibleContexts #-}
module Sodium.Haskell.Convert (convert) where

import Sodium.Prelude

import Data.Char (isAlphaNum, isLetter)
import qualified Data.Map as M

import Sodium.Haskell.Common
import qualified Sodium.Nucleus.Vector.Program as S
import qualified Language.Haskell.Exts        as H
import qualified Language.Haskell.Exts.SrcLoc as H

convert :: Map Integer String -> S.Program -> H.Module
convert = convProgram

type T m = (Applicative m, MonadState (Map Integer (Either String H.Name)) m)

convProgram :: Map Integer String -> S.Program -> H.Module
convProgram nameTags (S.Program funcs) = (`evalState` fmap Left nameTags) $ do
    (concat -> hsDecls) <- traverse (uncurry convFunc) (itoList funcs)
    let hsExts = extensions ["MultiWayIf", "ScopedTypeVariables"]
        hsMod = H.Module H.noLoc H.main_mod hsExts Nothing Nothing [] hsDecls
    pure hsMod
  where extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]

keywords :: [String]
keywords = words
    " _ as case class data default deriving do else hiding \
    \   if import in infix infixl infixr instance let main \
    \   module newtype of qualified then type where        "

nameSafe :: String -> String
nameSafe = kwClash . startsNice . filter (liftA2 (||) isAlphaNum (=='_')) where
    startsNice (c:cs) | isLetter c = c:cs
    startsNice cs = "_" ++ cs
    kwClash cs | cs `elem` keywords = "_" ++ cs
               | otherwise = cs

-- TODO: handle name shadowing and other conflicts
-- (nameGenAccess and nameGenBind?)
nameGen :: T m => Integer -> m H.Name
nameGen n = gets (M.lookup n) >>= \case
    Nothing -> do
        modify $ M.insert n (Left (show n))
        nameGen n
    Just (Left str) -> do
        modify $ M.insert n (Right (H.Ident (nameSafe str)))
        nameGen n
    Just (Right name) -> pure name

convType :: S.Type -> H.Type
convType = \case
    S.TypeInteger -> H.TyCon (HsIdent "Prelude" "Int")
    S.TypeDouble  -> H.TyCon (HsIdent "Prelude" "Double")
    S.TypeBoolean -> H.TyCon (HsIdent "Prelude" "Bool")
    S.TypeChar    -> H.TyCon (HsIdent "Prelude" "Char")
    S.TypeUnit    -> H.TyCon (H.Special H.UnitCon)
    S.TypePair t1 t2  -> H.TyTuple H.Boxed [convType t1, convType t2]
    S.TypeList S.TypeChar -> H.TyCon (HsIdent "Prelude" "String")
    S.TypeList ts -> H.TyList (convType ts)
    S.TypeFunction t1 t2 -> H.TyFun (convType t1) (convType t2)
    S.TypeTaint t -> H.TyApp (H.TyCon (HsIdent "Prelude" "IO")) (convType t)


convExpression :: T m => S.Expression -> m H.Exp
convExpression = \case
    S.Lambda pat act
         -> H.Lambda H.noLoc
        <$> traverse convPattern [pat]
        <*> convExpression act
    S.Beta a1 a2 -> H.App <$> convExpression a1 <*> convExpression a2
    S.Primary lit -> pure $ convLit lit
    S.Access name -> case name of
        S.NameSpecial op -> pure $ convOp op
        S.NameGen n -> nameGen n <&> \hsName -> H.Var (H.UnQual hsName)

convFunc :: T m => S.Name -> S.Func -> m [H.Decl]
convFunc name (S.Func ty expression) = do
    hsName <- case name of
        S.NameSpecial S.OpMain -> pure H.main_name
        S.NameGen n -> nameGen n
        _ -> error "convFunc: incorrect name"
    let sig = H.TypeSig H.noLoc [hsName] (convType ty)
    hsRhs <- H.UnGuardedRhs <$> convExpression expression
    let funBind = H.FunBind [H.Match H.noLoc hsName [] Nothing hsRhs (H.BDecls [])]
    pure [sig, funBind]

convPattern :: T m => S.Pattern -> m H.Pat
convPattern = \case
    S.PUnit -> pure $ H.PTuple H.Boxed []
    S.PWildCard -> pure $ H.PWildCard
    S.PAccess name ty -> do
        hsName <- case name of
            S.NameGen n -> nameGen n
            _ -> error "convPattern: incorrect name"
        let annotate pat = H.PatTypeSig H.noLoc pat (convType ty)
        pure $ annotate (H.PVar hsName)
    S.PTuple pat1 pat2 -> H.PTuple H.Boxed <$> traverse convPattern [pat1, pat2]

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
    S.OpNegate   -> H.Var (HsIdent "Prelude" "negate")
    S.OpShow     -> H.Var (HsIdent "Prelude" "show")
    S.OpIf       -> H.Var (HsIdent "Data.Bool" "bool")
    S.OpFold     -> H.Var (HsIdent "Prelude" "foldl")
    S.OpFoldTainted -> H.Var (HsIdent "Control.Monad" "foldM")
    S.OpMapTaintedIgnore -> H.Var (HsIdent "Data.Foldable" "traverse_")
    S.OpProduct  -> H.Var (HsIdent "Prelude" "product")
    S.OpSum      -> H.Var (HsIdent "Prelude" "sum")
    S.OpAnd'     -> H.Var (HsIdent "Prelude" "and")
    S.OpOr'      -> H.Var (HsIdent "Prelude" "or")
    S.OpAdd      -> H.Var (HsSymbol "Prelude" "+")
    S.OpSubtract -> H.Var (HsSymbol "Prelude" "-")
    S.OpMultiply -> H.Var (HsSymbol "Prelude" "*")
    S.OpDivide   -> H.Var (HsSymbol "Prelude" "/")
    S.OpDiv      -> H.Var (HsIdent "Prelude" "div")
    S.OpMod      -> H.Var (HsIdent "Prelude" "mod")
    S.OpMore     -> H.Var (HsSymbol "Prelude" ">")
    S.OpLess     -> H.Var (HsSymbol "Prelude" "<")
    S.OpMoreEquals -> H.Var (HsSymbol "Prelude" ">=")
    S.OpLessEquals -> H.Var (HsSymbol "Prelude" "<=")
    S.OpEquals   -> H.Var (HsSymbol "Prelude" "==")
    S.OpNotEquals-> H.Var (HsSymbol "Prelude" "/=")
    S.OpTrue     -> H.Con (HsIdent "Prelude" "True")
    S.OpFalse    -> H.Con (HsIdent "Prelude" "False")
    S.OpAnd      -> H.Var (HsSymbol "Prelude" "&&")
    S.OpOr       -> H.Var (HsSymbol "Prelude" "||")
    S.OpNot      -> H.Var (HsIdent "Prelude" "not")
    S.OpElem     -> H.Var (HsIdent "Prelude" "elem")
    S.OpRange    -> H.Var (HsIdent "Prelude" "enumFromTo")
    S.OpId       -> H.Var (HsIdent "Prelude" "id")
    S.OpFst      -> H.Var (HsIdent "Prelude" "fst")
    S.OpSnd      -> H.Var (HsIdent "Prelude" "snd")
    S.OpSwap     -> H.Var (HsIdent "Data.Tuple" "swap")
    S.OpPutLn    -> H.Var (HsIdent "Prelude" "putStrLn")
    S.OpPut      -> H.Var (HsIdent "Prelude" "putStr")
    S.OpGetLn    -> H.Var (HsIdent "Prelude" "getLine")
    S.OpReadLn   -> H.Var (HsIdent "Prelude" "readLn")
    S.OpPrintLn  -> H.Var (HsIdent "Prelude" "print")
    S.OpConcat   -> H.Var (HsSymbol "Prelude" "++")
    S.OpBind      -> H.Var (HsSymbol "Prelude" ">>=")
    S.OpBindIgnore-> H.Var (HsSymbol "Prelude" ">>")
    S.OpFmapIgnore-> H.Var (HsSymbol "Control.Applicative" "<$")
    S.OpIgnore    -> H.Var (HsIdent "Control.Monad" "void")
    S.OpTaint     -> H.Var (HsIdent "Prelude" "return")
    S.OpIntToDouble -> H.Var (HsIdent "Prelude" "fromIntegral")
    S.OpUndefined -> H.Var (HsIdent "Prelude" "undefined")
    S.OpMain -> H.Var (H.UnQual (H.main_name))
