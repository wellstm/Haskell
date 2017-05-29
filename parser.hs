-- Parser Stuff!
import Data.Char

type Parser a = String -> [(a, String)]

-- Parser that always succeeds
return :: a -> Parser a
return v = \input -> [(v, input)]

-- Parser that always fails
failure :: Parser a
failure = \x -> []

-- Parser that parses a single char
item :: Parser Char
item = \x -> case x of
                    [] -> []
                    (x:xs) -> [(x,xs)]

-- Function for applying a parser to input
parse :: Parser a -> String -> [(a, String)]
parse p toParse = p toParse

-- Bind, apply after if the first one worked
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \input -> case parse p input of
                                        [] -> []
                                        [(v, out)] -> parse (f v) out

-- Choice, apply after if the first one didn't work
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \input -> case parse p input of
                                        [] -> parse q input
                                        [(v, out)] -> [(v, out)]

-- Some shitty test thing
test :: Parser(Char,Char)
test =  item Main.>>= \x1 ->
        item Main.>>= \_ ->
        item Main.>>= \x3 ->
        Main.return (x1, x3)
    

-- Parser for satisfying some predicate
sat :: (Char -> Bool) -> Parser Char
sat p = item Main.>>= \x1 ->
        if p x1 then Main.return x1 else failure


-- Useful parsers!
digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphaNum = sat isAlphaNum
char x = sat (\x1 -> x1 == x)

-- Parse a whole string
string :: String -> Parser String
string [] = Main.return []
string (x:xs) = (char x) Main.>>= \x1 ->
                (string xs) Main.>>= \x2 ->
                Main.return (x1:x2)

-- Apply a parser many times
many :: Parser a -> Parser [a]
many p = many1 p Main.+++ Main.return []

-- Apply a parser many times, but require it work at least once
many1  :: Parser a -> Parser [a]
many1 p =   p Main.>>= \x1 ->
            many p Main.>>= \x2 ->
            Main.return (x1:x2)

-- Parse identifiers
ident :: Parser String
ident = lower Main.>>= \x1 ->
        (many alphaNum) Main.>>= \x2 ->
        Main.return (x1:x2)

-- Parse natural numbers
nat :: Parser Int
nat =   many1 digit Main.>>= \x1 ->
        Main.return (read x1)

-- Parse spaces
space :: Parser ()
space = many (sat isSpace) Main.>>= \x1 ->
        Main.return ()

-- Parse a token surrounded by spaces
token :: Parser a -> Parser a
token p =   space Main.>>= \_ ->
            p Main.>>= \x2 ->
            space Main.>>= \_ ->
            Main.return x2

-- Wrap out helpers with the string handling code
identifier = token ident
natural = token nat
symbol xs = token (string xs)

innerList :: Parser Int
innerList =     (symbol ",") Main.>>= \_ ->
                natural

-- -- List Parser
list :: Parser [Int]
list =  (symbol "[") Main.>>= \_ ->
        natural Main.>>= \n ->
        many innerList Main.>>= \ns ->
        (symbol "]") Main.>>= \_ ->
        Main.return (n:ns)
