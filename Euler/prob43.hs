{-
    Problem 43 - Sub-string divisibility

    There were two keys to doing this problem with some measure of efficiency.

    First, d6 must be 5, since d4 d5 d6 must be divisible by 5 and d6 cannot be zero, otherwise
    d7 == d8 by virtue of d6 d7 d8 being divisible by 11.

    Second, with some experimentation, it becomes clear the chaining "backwards" from the toughest
    restriction first (divisible by 17, 13, ...) leads to a very small set of possibilities to check,
    at least going back to d6 = 5.  In fact, it turns out there are only 3 valid possibilities for the
    last 5 digits.  With those in mind, we can generate the 5! = 120 permutations of missing digits for
    each of the 3 possibilities and check the remaining sub-string divisibility properties (2, 3, 7)
    in a brute force fashion.

    Finally, we can slightly reduce the combinatorial possibilities by dropping 5 from the digit list.
-}

import Data.List


digitList = [0..9] \\ [5]

goodLastDigits =
    [ [5, d7, d8, d9, d10] | d7 <- digitList,
                             d8 <- digitList,
                             d9 <- digitList,
                             d10 <- digitList,
                             mod (d8 * 100 + d9 * 10 + d10 ) 17 == 0,
                             mod (d7 * 100 + d8 * 10 + d9) 13 == 0,
                             mod ( 500 + d7 * 10 + d8) 11 == 0,
                             d7 /= d8,
                             d7 /= d9,
                             d7 /= d10,
                             d8 /= d9,
                             d8 /= d10,
                             d9 /= d10 ]

listPairs =
    [ (l2, l1) | l1 <- goodLastDigits, l2 <- permutations (digitList \\ l1) ]

goodListPair :: ([Int], [Int]) -> Bool
goodListPair
    ([d1, d2, d3, d4, d5], [d6, d7, _, _, _]) =
        (mod d4 2 == 0) && (mod (d3 + d4 + d5) 3 == 0) && (mod (d5 * 100 + 50 + d7) 7 == 0)

goodPairs = filter goodListPair listPairs

listPairToList :: ([Int], [Int]) -> [Integer]
listPairToList
    (l1, l2) = (map toInteger l1) ++ (map toInteger l2)

buildNum :: Integer -> Integer -> Integer
buildNum n1 n2 = 10 * n1 + n2

listToNumber :: [Integer] -> Integer
listToNumber l =
    foldl buildNum 0 l

prob43 = sum $ map (listToNumber . listPairToList) goodPairs
