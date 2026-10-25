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
    plus,
    letter,
    alphanum,
    word,
    many,
    integer,
    parenthesized,
) where

import Control.Applicative (Alternative (empty, (<|>)))

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
word = neWord <|> result ""
  where
    neWord = do
        x  <- letter
        xs <- word
        result (x : xs)

-- parser many items of type p

many :: Parser a -> Parser [a]
many p = manyList <|> result []
  where
    manyList = do
        x  <- p
        xs <- (many p)
        result (x : xs)
                        
integer :: Parser Int
integer = do
    digits <- many digit
    pure (read digits :: Int)

-- wraps a parser in parenthesises
parenthesized :: Parser a -> Parser a
parenthesized p = do
    _ <- char '('
    x <- p
    _ <- char ')'
    pure x