module Sodium.Pascal.Tokenize (tokenCC, Token(..)) where

import Data.Functor
import Control.Monad

import Data.Char (toLower)
import qualified Data.HashMap.Strict as M

import Text.Parsec

data Token
    = KwVar
    | KwBegin
    | KwEnd
    | KwFor
    | KwTo
    | KwDo
    | KwFunction
    | KwTrue
    | KwFalse
    | KwAnd
    | KwOr
    | KwIf
    | KwThen
    | KwElse
    | KwCase
    | KwOf
    | LParen
    | RParen
    | Semicolon
    | Comma
    | Dot
    | DoubleDot
    | Plus
    | Minus
    | Assign
    | Asterisk
    | Slash
    | Colon
    | EqSign
    | Suck
    | Blow
    | Name String
    | INumber String
    | FNumber String String
    | ENumber String String Bool String
    | Quote String
    | LSqBrace
    | RSqBrace
    | EOF
    deriving (Eq, Show)

type P u a = Parsec String u a

tokenCC :: (Token -> P u a) -> P u a
tokenCC cont = pToken' >>= cont

pToken' :: P u Token
pToken' = do pSkip
             pToken <|> pEOF

pEOF :: P u Token
pEOF = EOF <$ eof

pToken :: P u Token
pToken = choice [pPunct, pNumber, pName, pQuote] <?> "token"

pPunct :: P u Token
pPunct = choice
   [ char '(' >> choice
        [ char '.' $> LSqBrace
        , return LParen
        ]
   , char ')' $> RParen
   , char '[' $> LSqBrace
   , char ']' $> RSqBrace
   , char '+' $> Plus
   , char '-' $> Minus
   , char '*' $> Asterisk
   , char '/' $> Slash
   , char ',' $> Comma
   , char ';' $> Semicolon
   , char '=' $> EqSign
   , char '<' $> Suck
   , char '>' $> Blow
   , char '.' >> choice
        [ char '.' $> DoubleDot
        , char ')' $> RSqBrace
        , return Dot
        ]
   , char ':' >> choice
        [ char '=' $> Assign
        , return Colon
        ]
   ]

pSign :: P u Bool
pSign = choice
      [ char '+' $> True
      , char '-' $> False
      , return True
      ]

pNumber :: P u Token
pNumber = do
    intSection <- many1 digit
    try (pFloat intSection)
        <|> return (INumber intSection)

pFloat :: String -> P u Token
pFloat intSection = do
    char '.'
    fracSection <- many1 digit
    try (pExp intSection fracSection)
        <|> return (FNumber intSection fracSection)

pExp :: String -> String -> P u Token
pExp intSection fracSection = do
    char 'e' <|> char 'E'
    eSign <- pSign
    eSection <- many1 digit
    return (ENumber intSection fracSection eSign eSection)

pName :: P u Token
pName = let gen p = char '_' <|> fmap toLower p
            ident = liftM2 (:) (gen letter) (many $ gen alphaNum)
        in fmap mangle ident

mangle :: String -> Token
mangle cs = maybe (Name cs) id (M.lookup cs keywords)

keywords :: M.HashMap String Token
keywords = M.fromList
    [ ("var"     , KwVar)
    , ("begin"   , KwBegin)
    , ("end"     , KwEnd)
    , ("for"     , KwFor)
    , ("to"      , KwTo)
    , ("do"      , KwDo)
    , ("function", KwFunction)
    , ("true"    , KwTrue)
    , ("false"   , KwFalse)
    , ("and"     , KwAnd)
    , ("or"      , KwOr)
    , ("if"      , KwIf)
    , ("then"    , KwThen)
    , ("else"    , KwElse)
    , ("case"    , KwCase)
    , ("of"      , KwOf)
    ]

pQuote :: P u Token
pQuote = fmap Quote (char qmark >> quote') where
    qmark  = '\''
    quote' = anyChar >>= next
    next c | c == qmark = liftM2 (:) (char qmark) quote' <|> return ""
           | otherwise  = liftM2 (:) (return c)   quote'

pSkip :: P u ()
pSkip = skipMany (void space <|> pComment)

pComment :: P u ()
pComment = void comment <?> "comment"
         where comment = open >> manyTill anyChar close
               open  = void (char '{') <|> void (try $ string "(*")
               close = void (char '}') <|> void (try $ string "*)")
