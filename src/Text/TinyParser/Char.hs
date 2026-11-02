module Text.TinyParser.Char (
    sat,
    char,
    digit,
    upper,
    lower,
    whitespace,
    letter,
    alphanum,
    word,
    nat,
    int,
    ident,
    ints,
    parenthesized,
    escape,
) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isSpace)
import Text.TinyParser.Combinators
import Text.TinyParser.Core

-- simple parsers
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
        then result x
        else zero

-- some nice parsers using sat
char :: Char -> Parser Char
char x = sat $ (== x)

digit :: Parser Char
digit = sat $ \x -> '0' <= x && x <= '9'

upper :: Parser Char
upper = sat $ \x -> 'A' <= x && x <= 'Z'

lower :: Parser Char
lower = sat $ \x -> 'a' <= x && x <= 'z'

whitespace :: Parser Char
whitespace = sat $ isSpace

-- parsers using plus
letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

-- accepts strings of only letters
word :: Parser String
word = some letter

-- more complex basic parsers

-- natural number
nat :: Parser Int
nat = do
    digits <- some digit
    pure (read digits :: Int)

-- integer
int :: Parser Int
int = negative <|> nat
  where
    negative = do
        _ <- char '-'
        n <- nat
        pure (-n)

-- reads an identifier
ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    pure (x : xs)

-- parses a list of integers separated by comma [1 ,5 ,6]
ints :: Parser [Int]
ints = do
    _ <- char '['
    xs <- sepby1 (escape int) (char ',')
    _ <- escape (char ']')
    pure xs

-- wraps a parser in parentheses
parenthesized :: Parser a -> Parser a
parenthesized p = do
    _ <- escape $ char '('
    x <- p
    _ <- escape $ char ')'
    pure x

escape :: Parser a -> Parser a
escape p = do
    _ <- many whitespace
    p
