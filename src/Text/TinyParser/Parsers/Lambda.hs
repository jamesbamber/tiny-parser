module Text.TinyParser.Parsers.Lambda where

import Text.TinyParser.Char
import Text.TinyParser.Combinators
import Text.TinyParser.Core

import Control.Applicative (Alternative ((<|>)))

-- BNF grammar for lambda expression parser
-- expr =  expr ' ' expr | atom
-- atom = ( expr ) | var | \var.expr

data Expr
    = Evar String
    | Eabs String Expr
    | Eapp Expr Expr
    deriving (Show, Eq)

expr :: Parser Expr
expr = atom `chainl1` applicationOp
  where
    applicationOp = do
        _ <- char ' '
        pure Eapp

atom :: Parser Expr
atom = parenthesized expr <|> abstraction <|> variable
  where
    abstraction = do
        _ <- escape (char '\\')
        v <- escape ident
        _ <- escape (char '.')
        e <- expr
        pure (Eabs v e)
    variable = Evar <$> escape ident
