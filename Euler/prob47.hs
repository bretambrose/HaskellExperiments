{-
    Problem 47 - Distinct Primes Factors


-}

import qualified Data.Map as Map


{- Inefficient prime generation, taken from http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf -}

sieve :: [Int] -> [Int]
sieve l = sieve' l Map.empty
    where
        sieve' [] _ = []
        sieve' (x:xs) table =
            case Map.lookup x table of
                Nothing -> x : sieve' xs (Map.insert (x * x) [x] table)
                Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
                    where
                        reinsert table p = Map.insertWith (++) (x + p) [p] table

generatePrimeList :: [Int]
generatePrimeList = sieve [2..]

{- Problem solution -}

factorNumberAux :: Int -> Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
factorNumberAux 1 p pcount _ fl =
    if pcount > 0 then
        (p, pcount) : fl
    else
        fl

factorNumberAux n p pcount pl@(px:pxs) fl =
    if mod n p == 0 then
        factorNumberAux (div n p) p (pcount + 1) pl fl
    else if pcount > 0 then
        factorNumberAux n px 0 pxs $ (p, pcount):fl
    else
        factorNumberAux n px 0 pxs fl

factorNumber :: Int -> [Int] -> [(Int, Int)]
factorNumber n pl
    | n < 2 = []
    | otherwise = factorNumberAux n (head pl) 0 (tail pl) []

{- Try 2 -}

reduceBy :: Int -> Int -> Int
reduceBy n p =
    let n' = div n p in
        if n' * p == n then
            reduceBy n' p
        else
            n

hasFourFactors :: Int -> [Int] -> Int ->  Bool
hasFourFactors fcount pl 1 = fcount == 4
hasFourFactors fcount (px : pxs) n =
    let n' = reduceBy n px in
        if n /= n' then
            if n' > 1 && fcount >= 3 then
                False
            else
                hasFourFactors (fcount + 1) pxs n'
        else
            hasFourFactors fcount pxs n

fourConsecutiveFourFactorsAux :: Int -> Int -> [Int] -> Int
fourConsecutiveFourFactorsAux start current pl =
    if hasFourFactors 0 pl current then
        if current - start >= 3 then
            start
        else
            fourConsecutiveFourFactorsAux start (current + 1) pl
    else
        fourConsecutiveFourFactorsAux (current + 1) (current + 1) pl

fourConsecutiveFourFactors =
    let pl = generatePrimeList in
        fourConsecutiveFourFactorsAux 647 647 pl

