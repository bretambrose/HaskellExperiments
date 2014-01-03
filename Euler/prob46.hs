{-
    Problem 46 - Goldbach's Other Conjecture

    You need to install the primes package at
     http://hackage.haskell.org/package/primes-0.1.1/docs/Data-Numbers-Primes.html

-}

import qualified Data.Numbers.Primes as Primes

isNotGoldbach :: [Integer] -> Integer -> Bool
isNotGoldbach (px : pxs) n =
    if px == n then
        False
    else if px > n then
        True
    else
        let diff = n - px
            sqr = round $ sqrt $ fromIntegral $ div diff 2 in
            if n == px + sqr * sqr * 2 then
                False
            else
                isNotGoldbach pxs n

prob46 :: Integer
prob46 =
    let pl = Primes.primes in
        head $ filter (isNotGoldbach pl) [9,11..]

