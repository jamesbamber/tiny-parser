import Parser

import Control.Applicative (Alternative (empty, (<|>)))

-- BNF grammar for expr parser
-- expr n = (expr n-1) op (expr n-1) | expr
-- atom = ( expr ) | integer

-- in this case
-- op 2  = (+) | (-)
-- op 1  = (*) | (/)

-- to avoid left recursion in parsing expr = expr op expr 
-- we view it as expr = atom (many (op atom)) and then foldl

highestLevel = 3

expr :: Parser Int
expr = exprLevel highestLevel

exprLevel :: Int -> Parser Int
exprLevel 0 = atom
exprLevel n = do
    x   <- exprLevel (n-1)
    fxs <- many $ do
        f <- op (n)
        y <- exprLevel (n-1)
        pure (f, y)
    pure $ foldl (\acc (f, y) -> f acc y) x fxs

atom :: Parser Int
atom = parenthesized expr <|> escape integer

op :: Int -> Parser (Int  -> Int -> Int)
op 3 = addition <|> subtraction
    where
        addition = do
            escape $ char '+'
            pure (+)
        subtraction = do
            escape $ char '-'
            pure (-)
op 2 = multiplication <|> division
    where
        multiplication = do
            escape $ char '*'
            pure (*)
        division = do
            escape $ char '/'
            pure div
op 1 = exponentiation
    where
        exponentiation = do
            escape $ char '^'
            pure (^)
op _ = empty
