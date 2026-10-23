type Parser a = String -> [(a, String)]


-- primitive parsers
result :: a -> Parser a
result v = \inp -> [(v, inp)]

zero :: Parser a
zero = \inp -> []

item :: Parser Char
item = \inp -> case inp of
    [] -> []
    (x : xs) -> [(x, xs)]

-- combines two parsers (causes nested tuples)
seq :: Parser a -> Parser b -> Parser (a, b)
p `seq` q = \inp -> [((v, w), inp'') | (v, inp') <- p inp, (w, inp'') <- q inp']

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v, inp') <- p inp]

-- simple parsers
sat :: (Char -> Bool) -> Parser Char
sat p =
    item `bind` \x ->
        if p x then result x else zero

-- some nice parsers using sat
char :: Char -> Parser Char
char x = sat $ \y -> x == y

digit :: Parser Char
digit = sat $ \x -> '0' <= x && x <= '9'

upper :: Parser Char
upper = sat $ \x -> 'A' <= x && x <= 'Z'

lower :: Parser Char
lower = sat $ \x -> 'a' <= x && x <= 'z'

-- choice operator plus (genius)
plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp -> p inp ++ q inp

-- parsers using plus
letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

-- accepts strings of only letters
word :: Parser String
word = neWord `plus` result ""
  where
    neWord =
        letter `bind` \x ->
            word `bind` \xs ->
                result (x : xs)

-- parser many items of type p

many :: Parser a -> Parser [a]
many p = manyList `plus` result []
  where
    manyList =
        p `bind` \x ->
            (many p) `bind` \xs ->
                result (x : xs)
                        