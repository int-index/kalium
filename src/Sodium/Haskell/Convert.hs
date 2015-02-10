{-# LANGUAGE FlexibleContexts #-}
module Sodium.Haskell.Convert (convert, Config(..)) where

import Sodium.Prelude

import Data.Char (isAlphaNum, isLetter)
import qualified Data.Map as M
import qualified Data.Set as S

import Sodium.Haskell.Common
import qualified Sodium.Nucleus.Vector.Program as Vec
import qualified Sodium.Nucleus.Vector.Pattern as Vec
import qualified Language.Haskell.Exts        as H
import qualified Language.Haskell.Exts.SrcLoc as H

convert :: Config -> Map Integer String -> Vec.Program -> H.Module
convert = convProgram

data Config = Config
    { configPatSig :: Bool
    }

type T m =
    ( Applicative m
    , MonadReader (Set Vec.Name) m
    , MonadState (Map Integer (Either String H.Name)) m )
type C m = (Applicative m, MonadConfig Config m)

convProgram :: Config -> Map Integer String -> Vec.Program -> H.Module
convProgram config nameTags (Vec.Program funcs) = run $ do
    (concat -> hsDecls) <- traverse (uncurry convFunc) (itoList funcs)
    let hsExts = extensions ["MultiWayIf", "ScopedTypeVariables"]
        hsMod = H.Module H.noLoc H.main_mod hsExts Nothing Nothing [] hsDecls
    pure hsMod
  where
    extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]
    run = (`evalState` fmap Left nameTags)
        . (`runConfigT` config)
        . (`runReaderT` M.keysSet funcs)

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

resolveConflict :: H.Name -> Set H.Name -> H.Name
resolveConflict name names
    | name `S.notMember` names = name
    | otherwise = resolveConflict (change name) names
  where
    change (H.Ident str)  = H.Ident (str ++ "'")
    change _ = error "impossible happened: H.Ident expected"

nameGen :: T m => Integer -> m H.Name
nameGen n = gets (M.lookup n) >>= \case
    Nothing -> do
        modify $ M.insert n (Left (show n))
        nameGen n
    Just (Left str) -> do
        let name = H.Ident (nameSafe str)
        -- TODO: take only names in current scope
        names <- do
            dict <- get
            let assocName (Vec.NameGen n)
                  | Just (Right name) <- M.lookup n dict
                  = S.insert name
                assocName _ = id
            asks $ S.foldr assocName S.empty
        modify $ M.insert n (Right $ resolveConflict name names)
        nameGen n
    Just (Right name) -> pure name

convType :: Vec.Type -> H.Type
convType = \case
    Vec.TypeInteger -> H.TyCon (HsIdent "Prelude" "Int")
    Vec.TypeDouble  -> H.TyCon (HsIdent "Prelude" "Double")
    Vec.TypeBoolean -> H.TyCon (HsIdent "Prelude" "Bool")
    Vec.TypeChar    -> H.TyCon (HsIdent "Prelude" "Char")
    Vec.TypeTaint   -> H.TyCon (HsIdent "Prelude" "IO")
    Vec.TypeUnit    -> H.TyCon (H.Special H.UnitCon)
    Vec.TypePair    -> H.TyCon (H.Special (H.TupleCon H.Boxed 2))
    Vec.TypeFunction-> H.TyCon (H.Special H.FunCon)
    Vec.TypeList    -> H.TyCon (H.Special H.ListCon)
    Vec.TypeBeta t1 t2 -> H.TyApp (convType t1) (convType t2)
    _ -> error "convType: unknown type"


convExpression :: (T m, C m) => Vec.Expression -> m H.Exp
convExpression = \case
    Vec.Lambda pat act
         -> H.Lambda H.noLoc
        <$> traverse convPattern [pat]
        <*> local (Vec.patBound pat <>) (convExpression act)
    Vec.Beta a1 a2 -> H.App <$> convExpression a1 <*> convExpression a2
    Vec.Primary lit -> pure $ convLit lit
    Vec.Access name -> case name of
        Vec.NameSpecial op -> pure $ convOp op
        Vec.NameGen n -> nameGen n <&> \hsName -> H.Var (H.UnQual hsName)

convFunc :: (T m, C m) => Vec.Name -> Vec.Func -> m [H.Decl]
convFunc name (Vec.Func ty expression) = do
    hsName <- case name of
        Vec.NameSpecial Vec.OpMain -> pure H.main_name
        Vec.NameGen n -> nameGen n
        _ -> error "convFunc: incorrect name"
    let sig = H.TypeSig H.noLoc [hsName] (convType ty)
    hsRhs <- H.UnGuardedRhs <$> convExpression expression
    let funBind = H.FunBind [H.Match H.noLoc hsName [] Nothing hsRhs (H.BDecls [])]
    pure [sig, funBind]

convPattern :: (T m, C m) => Vec.Pattern -> m H.Pat
convPattern = \case
    Vec.PUnit -> pure $ H.PTuple H.Boxed []
    Vec.PWildCard -> pure $ H.PWildCard
    Vec.PAccess name ty -> do
        hsName <- case name of
            Vec.NameGen n -> nameGen n
            _ -> error "convPattern: incorrect name"
        configPatSig <- configPatSig <$> config
        let annotate
             | configPatSig = \pat -> H.PatTypeSig H.noLoc pat (convType ty)
             | otherwise = id
        pure $ annotate (H.PVar hsName)
    Vec.PTuple pat1 pat2 -> H.PTuple H.Boxed <$> traverse convPattern [pat1, pat2]

convLit :: Vec.Literal -> H.Exp
convLit = \case
    Vec.LitInteger n -> (if n < 0 then H.Paren else id) $ H.Lit (H.Int  n)
    Vec.LitDouble  x -> (if x < 0 then H.Paren else id) $ H.Lit (H.Frac x)
    Vec.LitChar    c -> H.Lit $ H.Char c

composeOp = H.Var (HsSymbol "Prelude" ".")

convOp :: Vec.NameSpecial -> H.Exp
convOp = \case
    Vec.OpSingleton -> H.RightSection
        (H.QConOp H.list_cons_name)
        (convOp Vec.OpNil)
    Vec.OpPair     -> H.tuple_con H.Boxed 1
    Vec.OpCons     -> H.Con H.list_cons_name
    Vec.OpUnit     -> H.unit_con
    Vec.OpNil      -> H.List []
    Vec.OpIx       -> H.Var (HsSymbol "Prelude" "!!")
    Vec.OpIxSet    -> composeOp
        `H.App` H.Var (HsSymbol "Control.Lens" "set")
        `H.App` H.Var (HsSymbol "Control.Lens" "ix")
    Vec.OpLength   -> H.Var (HsIdent "Prelude" "length")
    Vec.OpNegate   -> H.Var (HsIdent "Prelude" "negate")
    Vec.OpShow     -> H.Var (HsIdent "Prelude" "show")
    Vec.OpIf       -> H.Var (HsIdent "Data.Bool" "bool")
    Vec.OpFold     -> H.Var (HsIdent "Prelude" "foldl")
    Vec.OpFoldTainted -> H.Var (HsIdent "Control.Monad" "foldM")
    Vec.OpMapTaintedIgnore -> H.Var (HsIdent "Data.Foldable" "traverse_")
    Vec.OpProduct  -> H.Var (HsIdent "Prelude" "product")
    Vec.OpSum      -> H.Var (HsIdent "Prelude" "sum")
    Vec.OpAnd'     -> H.Var (HsIdent "Prelude" "and")
    Vec.OpOr'      -> H.Var (HsIdent "Prelude" "or")
    Vec.OpAdd      -> H.Var (HsSymbol "Prelude" "+")
    Vec.OpSubtract -> H.Var (HsSymbol "Prelude" "-")
    Vec.OpMultiply -> H.Var (HsSymbol "Prelude" "*")
    Vec.OpDivide   -> H.Var (HsSymbol "Prelude" "/")
    Vec.OpDiv      -> H.Var (HsIdent "Prelude" "div")
    Vec.OpMod      -> H.Var (HsIdent "Prelude" "mod")
    Vec.OpMore     -> H.Var (HsSymbol "Prelude" ">")
    Vec.OpLess     -> H.Var (HsSymbol "Prelude" "<")
    Vec.OpMoreEquals -> H.Var (HsSymbol "Prelude" ">=")
    Vec.OpLessEquals -> H.Var (HsSymbol "Prelude" "<=")
    Vec.OpEquals   -> H.Var (HsSymbol "Prelude" "==")
    Vec.OpNotEquals-> H.Var (HsSymbol "Prelude" "/=")
    Vec.OpTrue     -> H.Con (HsIdent "Prelude" "True")
    Vec.OpFalse    -> H.Con (HsIdent "Prelude" "False")
    Vec.OpAnd      -> H.Var (HsSymbol "Prelude" "&&")
    Vec.OpOr       -> H.Var (HsSymbol "Prelude" "||")
    Vec.OpNot      -> H.Var (HsIdent "Prelude" "not")
    Vec.OpElem     -> H.Var (HsIdent "Prelude" "elem")
    Vec.OpRange    -> H.Var (HsIdent "Prelude" "enumFromTo")
    Vec.OpId       -> H.Var (HsIdent "Prelude" "id")
    Vec.OpFst      -> H.Var (HsIdent "Prelude" "fst")
    Vec.OpSnd      -> H.Var (HsIdent "Prelude" "snd")
    Vec.OpSwap     -> H.Var (HsIdent "Data.Tuple" "swap")
    Vec.OpPutLn    -> H.Var (HsIdent "Prelude" "putStrLn")
    Vec.OpPut      -> H.Var (HsIdent "Prelude" "putStr")
    Vec.OpGetLn    -> H.Var (HsIdent "Prelude" "getLine")
    Vec.OpReadLn   -> H.Var (HsIdent "Prelude" "readLn")
    Vec.OpPrintLn  -> H.Var (HsIdent "Prelude" "print")
    Vec.OpConcat   -> H.Var (HsSymbol "Prelude" "++")
    Vec.OpTake     -> H.Var (HsIdent "Prelude" "take")
    Vec.OpRepeat   -> H.Var (HsIdent "Prelude" "repeat")
    Vec.OpBind      -> H.Var (HsSymbol "Prelude" ">>=")
    Vec.OpBindIgnore-> H.Var (HsSymbol "Prelude" ">>")
    Vec.OpFmapIgnore-> H.Var (HsSymbol "Control.Applicative" "<$")
    Vec.OpIgnore    -> H.Var (HsIdent "Control.Monad" "void")
    Vec.OpWhen      -> H.Var (HsIdent "Control.Monad" "when")
    Vec.OpTaint     -> H.Var (HsIdent "Prelude" "return")
    Vec.OpIntToDouble -> H.Var (HsIdent "Prelude" "fromIntegral")
    Vec.OpUndefined -> H.Var (HsIdent "Prelude" "undefined")
    Vec.OpMain -> H.Var (H.UnQual (H.main_name))
