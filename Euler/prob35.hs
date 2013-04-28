-- Project Euler, Problem 35 - Circular Primes
--
-- The main difficulty with this problem is learning enough Haskell (state monad) to implement an efficient
-- Sieve function

import Data.Array
import Data.Array.ST

num_digits_aux :: Int -> Int -> Int
num_digits_aux n d
    | n < 10 = d
    | otherwise = num_digits_aux (div n 10) (d + 1)

num_digits :: Int -> Int
num_digits n = num_digits_aux n 1

-- special case the numbers we'll actually use for efficiency

shift_number :: Int -> Int
shift_number n
    | n < 10 = n
    | n < 100 = (mod n 10) * 10 + n `div` 10
    | n < 1000 = (mod n 10) * 100 + n `div` 10
    | n < 10000 = (mod n 10) * 1000 + n `div` 10
    | n < 100000 = (mod n 10) * 10000 + n `div` 10
    | n < 1000000 = (mod n 10) * 100000 + n `div` 10
    | otherwise = let digits = (num_digits n) - 1 in
        (mod n 10) * (10 ^ digits) + n `div` 10

build_sieve :: Int -> Array Int Bool
build_sieve n =
    runSTArray( do
        {
            a <- newArray (0, n - 1) True;
            writeArray a 0 False;
            writeArray a 1 False;
            sequence[ writeArray a (d * i) False | d <- (2 : [3, 5..(round $ sqrt $ fromIntegral $ n - 1)]), i <- [2..(div (n-1) d)]];
            return a
        })

is_circular_prime :: Int -> Int -> Array Int Bool -> Bool
is_circular_prime n prime sieve
    | n == prime = True
    | otherwise =
          if sieve ! n == False then
              False
          else
              is_circular_prime (shift_number n) prime sieve

build_circular_prime_list :: Int -> [Int]
build_circular_prime_list n =
    let sieve = build_sieve n in
        [ p | p <- [2..(n-1)], sieve ! p, is_circular_prime (shift_number p) p sieve]

count_circular_primes :: Int -> Int
count_circular_primes n =
    length (build_circular_prime_list n)

