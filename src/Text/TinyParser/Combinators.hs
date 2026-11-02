module Text.TinyParser.Combinators (
    many,
    some,
    sepby,
    sepby1,
    chainl,
    chainl1,
    chainr,
    chainr1,
) where

import Control.Applicative (Alternative ((<|>)))
import Text.TinyParser.Core

-- parser many items of type p
many :: Parser a -> Parser [a]
many p = manyList <|> pure []
  where
    manyList = do
        x <- p
        xs <- (many p)
        pure (x : xs)

-- parsers at least one item of type p
some :: Parser a -> Parser [a]
some p = do
    x <- p
    xs <- (many p)
    pure (x : xs)

-- now we start cooking

-- parses a possibly empty list of a separated by b
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> (pure [])

-- parses a non-empty list of a separated by b
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
    x <- p
    xs <- many $ do
        _ <- sep
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
    pure $ foldl (\acc (f, y) -> f acc y) x fys

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
        f <- op
        xs <- p `chainr1` op
        pure $ f x xs
