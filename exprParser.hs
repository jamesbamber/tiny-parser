import Parser
import Control.Applicative (Alternative (empty, (<|>)))

maxLevel = 3

expr :: Parser Int
expr = exprLevel 0

exprLevel :: Int -> Parser Int
exprLevel n
    | n == maxLevel = atom
    | otherwise     = exprLevel (n+1) `chainl1` op (n)

atom :: Parser Int
atom = parenthesized (escape expr) <|> escape int

op :: Int -> Parser (Int  -> Int -> Int)
op 0 = addition <|> subtraction
  where
    addition = do
        escape $ char '+'
        pure (+)
    subtraction = do
        escape $ char '-'
        pure (-)
op 1 = multiplication <|> division
  where
    multiplication = do
        escape $ char '*'
        pure (*)
    division = do
        escape $ char '/'
        pure div
op 2 = exponentiation
  where
    exponentiation = do
        escape $ char '^'
        pure (^)
op _ = empty
