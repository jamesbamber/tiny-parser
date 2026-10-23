import Control.Monad (MonadPlus (..))

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Monad Parser where
    (>>=) = bind
    return = result

instance MonadPlus Parser where
    mzero = zero
    mplus = plus

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
p `plus` q = Parser $ \inp -> parse p inp ++ parse q inp

-- parsers using plus
letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

-- accepts strings of only letters
word :: Parser String
word = neWord `plus` result ""
  where
    neWord = do
        x  <- letter
        xs <- word
        result (x : xs)

-- parser many items of type p

many :: Parser a -> Parser [a]
many p = manyList `plus` result []
  where
    manyList = do
        x  <- p
        xs <- (many p)
        result (x : xs)
                        