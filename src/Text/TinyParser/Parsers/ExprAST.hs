module Text.TinyParser.Parsers.ExprAST (
    Expr (..),
    expr,
    eval,
) where

import Text.TinyParser.Char
import Text.TinyParser.Combinators
import Text.TinyParser.Core

import Control.Applicative (Alternative (empty, (<|>)))

data Expr
    = Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving (Show, Eq)

maxLevel :: Int
maxLevel = 3

expr :: Parser Expr
expr = exprLevel 0

exprLevel :: Int -> Parser Expr
exprLevel n
    | n == maxLevel = atom
    | n == 2 = exprLevel (n + 1) `chainr1` binOp (n) -- temporary fix because exponentiation is right associative
    | otherwise = exprLevel (n + 1) `chainl1` binOp (n)

atom :: Parser Expr
atom = parenthesized (escape expr) <|> literal
  where
    literal = do
        n <- escape int
        pure (Lit n)

binOp :: Int -> Parser (Expr -> Expr -> Expr)
binOp 0 = addition <|> subtraction
  where
    addition = do
        _ <- escape $ char '+'
        pure Add
    subtraction = do
        _ <- escape $ char '-'
        pure Sub
binOp 1 = multiplication <|> division
  where
    multiplication = do
        _ <- escape $ char '*'
        pure Mul
    division = do
        _ <- escape $ char '/'
        pure Div
binOp 2 = exponentiation
  where
    exponentiation = do
        _ <- escape $ char '^'
        pure Pow
binOp _ = empty

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
eval (Pow e1 e2) = eval e1 ^ eval e2
