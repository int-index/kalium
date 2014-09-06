module Sodium.Pascal.Tokenize (tokenize, Token(..)) where

import Prelude hiding (head)
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Lens (Cons, uncons)
import qualified Data.Char as C

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
    deriving (Eq, Show)

-- Useful combinators

head :: (Cons xs xs x x, MonadPlus m) => StateT xs m x
head = StateT $ \xs -> maybe mzero return (uncons xs)

fallback :: Alternative f => a -> f a -> f a
fallback = flip (<|>) . pure

expect :: (Cons xs xs x x, Eq x, MonadPlus m) => x -> StateT xs m x
expect x = mfilter (==x) head


-- Lexical definitions

tokenize :: String -> (String, [Token])
tokenize = runWriter . tokenize'

tokenize' :: String -> Writer [Token] String
tokenize' cs
    = maybe (return cs) (uncurry k) (runStateT token' cs)
    where k x cs = tell x >> tokenize' cs

token' :: Alternative f => StateT String Maybe (f Token)
token' = (pure <$> token) <|> (empty <$ ignored)

token :: StateT String Maybe Token
token
     =  LParen <$ expect '('
    <|> RParen <$ expect ')'
    <|> LSqBrace <$ expect '['
    <|> RSqBrace <$ expect ']'
    <|> Plus  <$ expect '+'
    <|> Minus <$ expect '-'
    <|> Asterisk <$ expect '*'
    <|> Slash <$ expect '/'
    <|> Comma <$ expect ','
    <|> Semicolon <$ expect ';'
    <|> EqSign <$ expect '='
    <|> Suck <$ expect '<'
    <|> Blow <$ expect '>'
    <|> expect '.' *> fallback Dot (DoubleDot <$ expect '.')
    <|> expect ':' *> fallback Colon (Assign <$ expect '=')
    <|> number
    <|> name
    <|> quote

ignored = void (some whitespace) <|> comment

name = mangle <$> some (letter <|> expect '_') where
    mangle cs = maybe (Name cs) id (lookup cs keywords)
    keywords =
        [ ("var", KwVar)
        , ("begin", KwBegin)
        , ("end", KwEnd)
        , ("for", KwFor)
        , ("to", KwTo)
        , ("do", KwDo)
        , ("function", KwFunction)
        , ("true", KwTrue)
        , ("false", KwFalse)
        , ("and", KwAnd)
        , ("or", KwOr)
        , ("if", KwIf)
        , ("then", KwThen)
        , ("else", KwElse)
        , ("case", KwCase)
        , ("of", KwOf)
        ]

number = do
    let sign = fallback True
             $ (True <$ expect '+') <|> (False <$ expect '-')
    intSection <- some digit
    fallback (INumber intSection) $ do
        expect '.'
        fracSection <- some digit
        fallback (FNumber intSection fracSection) $ do
            expect 'e'
            eSign <- sign
            eSection <- some digit
            return (ENumber intSection fracSection eSign eSection)

quote = Quote <$> (qmark *> quote') where
    qmark = expect '\''
    quote'
         =  qmark *> fallback "" (next qmark)
        <|> next head
    next x = (:) <$> x <*> quote'

comment = expect '{' *> comment' where
    comment'
         =  void (expect '}')
        <|> (comment <|> void head) *> comment'

whitespace = mfilter C.isSpace head

letter = C.toLower <$> mfilter C.isAlphaNum head

digit = mfilter C.isDigit head
