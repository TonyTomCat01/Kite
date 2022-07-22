{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Parser (
    parseHttp,
    parse,
    stringLiteral,
    string,
    char,
    request,
    host,
    keys,
) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Log

-- | NewType Declaration

-------------------------------------------

newtype Parser s = Parser
    { parse :: String -> Maybe (s, String)
    }
    deriving (Functor)

--------------------------------------------

-- | Instances

-----------------------------------------------------

instance Applicative Parser where
    pure x = Parser $ \i -> Just (x, i)
    (<*>) (Parser a) (Parser b) = Parser $ \i -> do
        (f, i') <- a i
        (a, i'') <- b i'
        Just (f a, i'')

instance Monad Parser where
    (>>=) (Parser a) f = Parser $ \i -> do
        (b, i') <- a i
        parse (f b) i'

instance Alternative Parser where
    empty = Parser (const Nothing)
    (<|>) (Parser a) (Parser b) = Parser $
        \i -> a i <|> b i

-----------------------------------------------------

-- Parsers

-----------------------------------------------------

item :: Parser Char
item = Parser $ \case
    (i : is) -> Just (i, is)
    _ -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
    c <- item
    if pred c then return c else empty

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

request =
    (\x _ y _ z _ -> (x, y, z))
        <$> method
        <*> satisfy isSpace
        <*> query
        <*> satisfy isSpace
        <*> version
        <*> (string "\r" <|> string "\n" <|> string "")
  where
    method =
        string "GET"
            <|> string "HEAD"
    query =
        many
            ( satisfy
                ( \x ->
                    isAlphaNum x
                        || x == '/'
                        || x == '.'
                )
            )
    version =
        many
            ( satisfy
                ( \x ->
                    isAlphaNum x
                        || x == '/'
                        || x == '.'
                )
            )

host =
    (\h _ i _ p _ -> (h, i, p))
        <$> string "Host"
        <*> string ": "
        <*> ip
        <*> char ':'
        <*> port
        <*> satisfy isSpace
  where
    ip =
        string "localhost"
            <|> many
                ( satisfy
                    ( \x ->
                        isDigit x
                            || x == '.'
                    )
                )
    port = many (satisfy isDigit)

keys =
    (\x _ y _ -> (x, y, ""))
        <$> stringLiteral
        <*> string ": "
        <*> stringLiteral
        <*> (string "\r" <|> string "\n" <|> string "")

stringLiteral =
    many
        ( satisfy (\x -> isAscii x && x /= ':' && (not . isSpace) x)
        )

------------------------------------------------------

parseHttp t =
    let Just (result, _) = parse ((,) <$> request <*> many keys) t
     in result
