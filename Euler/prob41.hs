-- Project Euler, Problem 41 - Pandigital Primes

{-



-}

import Data.List

is_prime_aux :: Int -> Int -> Int -> Bool
is_prime_aux n d max_d =
    if d > max_d then
        True
    else if (mod n d) == 0 then
        False
    else
        is_prime_aux n (d + 2) max_d


is_prime :: Int -> Bool
is_prime n
    | n <= 1 = False
    | otherwise = (mod n 2) == 1 && is_prime_aux n 3 (round $ sqrt $ fromIntegral n)

intListToNumAux :: [Int] -> Int -> Int
intListToNumAux [] r = r
intListToNumAux (x : xs ) r =
    intListToNumAux xs $ 10 * r + x

intListToNum :: [Int] -> Int
intListToNum l =
    intListToNumAux l 0


buildPandigitalPrimeList :: [Int] -> [Int] -> [Int]
buildPandigitalPrimeList nl rl =
    let n = intListToNum nl in
        if is_prime n then
            n : rl
        else
            rl

pandigitalPrimeListAux :: Int -> [Int]
pandigitalPrimeListAux n =
    filter is_prime $ map intListToNum (permutations [1 .. n])

-- the only candidate pandigital primes we need to check are 4, 5, 7, and 8.
-- 1 and 2 are obviously out, and 3, 6, and 9 pan-digits numbers all have digit sums
-- divisible by 3 which implies the numbers themselves are divisible by 3 and not prime

prob41 = maximum $ foldl (++) [] $ map pandigitalPrimeListAux [4, 5, 7, 8]

