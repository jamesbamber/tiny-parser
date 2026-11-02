module Text.TinyParser.Parsers.Expr where

import Text.TinyParser.Char
import Text.TinyParser.Combinators
import Text.TinyParser.Core

import Control.Applicative (Alternative (empty, (<|>)))

maxLevel :: Int
maxLevel = 3

expr :: Parser Int
expr = exprLevel 0

exprLevel :: Int -> Parser Int
exprLevel n
    | n == maxLevel = atom
    | otherwise = exprLevel (n + 1) `chainl1` op (n)

atom :: Parser Int
atom = parenthesized (escape expr) <|> escape int

op :: Int -> Parser (Int -> Int -> Int)
op 0 = addition <|> subtraction
  where
    addition = do
        _ <- escape $ char '+'
        pure (+)
    subtraction = do
        _ <- escape $ char '-'
        pure (-)
op 1 = multiplication <|> division
  where
    multiplication = do
        _ <- escape $ char '*'
        pure (*)
    division = do
        _ <- escape $ char '/'
        pure div
op 2 = exponentiation
  where
    exponentiation = do
        _ <- escape $ char '^'
        pure (^)
op _ = empty
