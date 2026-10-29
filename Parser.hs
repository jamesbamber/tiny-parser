module Parser (
    Parser (..),
    result,
    zero,
    item,
    bind,
    sat,
    char,
    digit,
    upper,
    lower,
    whitespace,
    plus,
    letter,
    alphanum,
    word,
    many,
    int,
    parenthesized,
    escape,
    sepby,
    sepby1,
    chainl,
    chainl1,
    chainr,
    chainr1
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isSpace)

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
    fmap f p = 
        p `bind` \a -> 
            result (f a)

instance Applicative Parser where
    pure = result
    p <*> q = 
        p `bind` \f -> 
            q `bind` \a -> 
                pure (f a)

instance Monad Parser where
    (>>=) = bind

instance Alternative Parser where
    empty = zero
    (<|>) = plus

-- primitive parsers
result :: a -> Parser a
result v = Parser $ \inp -> [(v, inp)]

zero :: Parser a
zero = Parser $ \inp -> []

item :: Parser Char
item = Parser $ \inp -> case inp of
    [] -> []
    (x : xs) -> [(x, xs)]

-- bind operator for monads
bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = Parser $ \inp -> concat [parse (f v) inp' | (v, inp') <- parse p inp]

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

-- choice operator plus (genius)
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp -> 
    case (parse p inp) of
        []  -> parse q inp
        res -> res

-- alternative nondeterministic plus (caused exponential complexity)
-- p `plus` q = Parser $ \inp -> parse p inp ++ parse q inp

-- parsers using plus
letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

-- accepts strings of only letters
word :: Parser String
word = neWord <|> pure ""
  where
    neWord = do
        x  <- letter
        xs <- word
        pure (x : xs)

-- parser many items of type p

many :: Parser a -> Parser [a]
many p = manyList <|> pure []
  where
    manyList = do
        x  <- p
        xs <- (many p)
        pure (x : xs)
                        
-- parsers at least 1 item of type p
some :: Parser a -> Parser [a]
some p = do
    x  <- p
    xs <- (many p)
    pure (x : xs)

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
        char '-'
        n <- nat
        pure (-n)

-- reads an identifier
ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    pure (x : xs)

-- wraps a parser in parentheses
parenthesized :: Parser a -> Parser a
parenthesized p = do
    _ <- escape $ char '('
    x <- p
    _ <- escape $ char ')'
    pure x

escape :: Parser a -> Parser a
escape p = do
    many whitespace
    p

-- now we start cooking

-- parses a possibly empty list of a separated by b
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> (pure [])

-- parses a non-empty list of a separated by b
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
    x <- p
    xs <- many $ do
        sep
        p
    pure (x : xs)


-- parses left associative operator chains, if empty returns v
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = (p `chainl1` op) <|> (pure v)

-- parses non-empty left associative operator chains
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
    x <- p
    fys <- many $ do
        f <- op
        y <- p
        pure (f, y)
    pure $ foldl(\acc (f, y) -> f acc y) x fys

-- parses right associative operator chains, if empty returns v
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = (p `chainr1` op) <|> (pure v)

-- parses non-empty right associative operator chains
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = do
    x <- p
    rest x <|> pure x
      where 
        rest x = do
            f  <- op
            xs <- p `chainr1` op
            pure $ f x xs


-- useful parsers

-- parses a list of integers separated by comma [1 ,5 ,6]
ints :: Parser [Int]
ints = do
    char '['
    xs <- sepby1 (escape int) (char ',')
    escape (char ']')
    pure xs
