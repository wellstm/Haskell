-- Ceasar Cipher Stuff

-- for ord
import Data.Char

-- Convert characters to int offsets
char2int :: Char -> Int
char2int c = ord c - ord 'a'

-- Convert int offsets to characters
int2char :: Int -> Char
int2char n = chr(ord 'a' + n)

-- Shift a single character by an offset
shift :: Int -> Char -> Char
shift n c   | isLower c = int2char ((char2int c + n) `mod` 26)
            | otherwise = c

-- Encode a string shifted by an offset
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs ]

-- ASCII Frequency Table from the internets
asciiFreqTable :: [Float]
asciiFreqTable = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
                  6.7, 7.5, 1.9, 0.1,  6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- Float division lol
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- Count the number of occurances of a character in a string
count :: Char -> String -> Int
count x xs = length [x' | x' <-xs, x' == x]

-- Count the number of lower case characters in a string
lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

-- Build a frequency table for a string
freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- ['a' .. 'z']]

-- Chi Square similarity of two frequency tables (lower is better)
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2) / e | (o, e) <- zip os es]

-- Rotate a list by an offset to the left
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- Give an element and a list, returns the positions that element occurs in the list.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                    where n = length xs - 1

-- Try to crack a Ceasar Cipher!
crack :: String -> String
crack xs = encode (-factor) xs
    where
        freqTable = freqs xs
        chitab = [chisqr (rotate n freqTable) asciiFreqTable | n <- [0..25]]
        factor = head (positions (minimum chitab) chitab)






