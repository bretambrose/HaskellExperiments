{-
    Problem 42 - Coded Triangle Numbers

    Determining if a number n is a triangle number requires testing if

    x^2 + x - 2n = 0

    has a positive integer-valued solution.

    From the quadratic equation we get

    s = (-1 +- sqrt( 1 + 8n )) / 2

    clearly we'll discard the negative sqrt term, leaving us with testing whether or not

    (-1 + sqrt(1 + 8n)) / 2

    is a positive integer, so we want

    sqrt(1 + 8n)

    to be an odd integer.  (1 + 8n) is always an odd integer, and the square root of an odd integer is
    always odd, so we really just need to test if sqrt(1 + 8n) is an integer.

    The edge case of n == 0 satisifies this test as well, but we ignore it.
-}

import Data.Char
import System.IO

calcCharNumber :: Char -> Int
calcCharNumber c
    | (c >= 'a' && c <= 'z') = 1 + (ord c - ord 'a')
    | (c >= 'A' && c <= 'Z') = 1 + (ord c - ord 'A')
    | otherwise = 0

calcWordNumberAux :: String -> Int -> Int
calcWordNumberAux [] r = r
calcWordNumberAux (x : xs ) r = calcWordNumberAux xs $ r + calcCharNumber x

calcWordNumber :: String -> Int
calcWordNumber l = calcWordNumberAux l 0

isTriangleNumber :: Int -> Bool
isTriangleNumber n
    | n <= 0 = False
    | otherwise =
        let squareTest = 1 + 8 * n in
            (round . sqrt $ fromIntegral squareTest) ^ 2 == squareTest

foldTriangleWord :: Int -> String -> Int
foldTriangleWord r word =
    if isTriangleNumber $ calcWordNumber word then
        1 + r
    else
        r

countTriangleWords :: [String] -> Int
countTriangleWords l =
    foldl foldTriangleWord 0 l

main = do
    wordFile <- readFile "prob42_words.txt"
    let wordList = "[" ++ wordFile ++ "]"   -- ugly
        words = read wordList :: [String] in
        putStrLn $ "Total number of triangle words: " ++ show (countTriangleWords words )

