module Text.TinyParser.Core (
    Parser (..),
    result,
    zero,
    item,
    bind,
) where

import Control.Applicative (Alternative (empty, (<|>)))

newtype Parser a = Parser {parse :: String -> [(a, String)]}

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
zero = Parser $ \_ -> []

item :: Parser Char
item = Parser $ \inp -> case inp of
    [] -> []
    (x : xs) -> [(x, xs)]

-- bind operator for monads
bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = Parser $ \inp -> concat [parse (f v) inp' | (v, inp') <- parse p inp]

-- choice operator plus
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp ->
    case (parse p inp) of
        [] -> parse q inp
        res -> res

-- alternative nondeterministic plus (caused exponential complexity)
-- p `plus` q = Parser $ \inp -> parse p inp ++ parse q inp
