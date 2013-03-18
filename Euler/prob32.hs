-- Problem 32, Project Euler
--
-- I was torn between two general approaches to this problem:
-- 1) Iterating permutations, or
-- 2) Iterating integer triples
--
-- I ended up using permutations, but it's not very efficient (5-6 second pause to compute the answer).
--
-- The primary simplifying observation is that the product MUST be 4 digits.
-- This can be inferred from the following two facts:
--   If the product were 5 or more digits, than we'd only have 4 digits for multipliers.  The largest product using only 4 digits is < 10000.
--   If the product were 3 or fewer digts, we'd have at least 6 digits for multipliers.  The smallest producting using 6 or more digits is >= 1000
--
-- This leads to a solution where we generate all permutations of "123456789" and check each permutation by breaking off the last 4 digits as
-- the product, while splitting the first 5 digits in two different ways (2,3) or (1,4).  Finally we check to see if either of the two multiplier pairs
-- produces the product.  Unfortunately, this isn't very efficient, since we're doing 4 expensive string-to-ints for each of 9! permutations.
--
-- More discussion below

import Data.List    -- for permutations

pandigital_product l =
    let product = read (drop 5 l) :: Int
        a = read (take 2 l) :: Int
        b = read (take 3 (drop 2 l)) :: Int
        c = read (take 1 l) :: Int
        d = read (take 4 (drop 1 l)) :: Int
    in if a * b == product || c * d == product then product else 0

pandigital_list_v1 = nub [ product | l <- permutations "123456789", let product = pandigital_product l, product > 0 ]

pandigital_sum_v1 = sum pandigital_list_v1

--
-- I think a more efficient implementation could be done by method 2:
--
-- Iterate over p = [1000..9999], m1 = [2..98], compute m2 = p / m1, and if m1 * m2 = p then check if the digits of p, m1, m2 are all distinct
-- This would be more efficient because the expensive part of the computation (digit uniqueness) is rarely needed (most things are filtered
-- by much quicker integer computations).
--
-- This ends up taking about two seconds, significantly better than v1, but still not real time unfortunately
has_zeros n
    | n < 10 = n == 0
    | otherwise = if (mod n 10 == 0) then True else has_zeros (div n 10)

digit_count_aux n d
    | n < 10 = d
    | otherwise = digit_count_aux (div n 10) (d + 1)

digit_count n =
    digit_count_aux n 1

is_pan_digital_aux p m1 m2 =
    length (nub ((show p) ++ (show m1) ++ (show m2))) == 9

is_pan_digital_tuple p m1 m2
    | m1 * m2 /= p = False
    | has_zeros m2 = False
    | m1_digits == 1 && m2_digits /= 4 = False
    | m1_digits == 2 && m2_digits /= 3 = False
    | otherwise = is_pan_digital_aux p m1 m2
    where m1_digits = digit_count m1
          m2_digits = digit_count m2

product_list = [ p | p <- [1234..9876], not (has_zeros p)]

m1_list = [m | m <- [2..98], not (has_zeros m)]

pandigital_list_v2 = nub [ p | p <- product_list, m1 <- m1_list, is_pan_digital_tuple p m1 (div p m1) ]

pandigital_sum_v2 = sum pandigital_list_v2

