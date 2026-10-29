import Parser

import Control.Applicative (Alternative (empty, (<|>)))

-- BNF grammar for this simple expr parser
-- expr = expr op expr | atom
-- atom = ( expr ) | integer
-- op = (+) | (-)

-- to avoid left recursion in parsing expr = expr op Exor 
-- we view it as expr = atom (many (op atom)) and then fold

expr :: Parser Int
expr = exprOpExpr <|> atom
  where 
    exprOpExpr = do
        x   <- atom
        fxs <- many $ do
            f <- op
            y <- atom
            pure (f, y)
        pure $ foldl (\acc (f, y) -> f acc y) x fxs

atom :: Parser Int
atom = parenthesized expr <|> int

op :: Parser (Int  -> Int -> Int)
op = do
    c <- (char '+' <|> char '-')
    case c of
        '+' -> pure (+)
        '-' -> pure (-)
