-- Project Euler, Problem 38 - Pandigital multiples

{-

We don't need to check n = 9 because the only integer that generates a pandigital number is 1, and it generates the smallest possible
pan-digital number and we know from the text that a larger one exists.

Let d be the pan-digital generator integer.

The concatenated product must be 9 digits and the individual products have a strictly non-decreasing length such that the maximum and minimum lengths
in the sequence can not be more than 1 apart (for a given d, 9 * d is at most one digit longer than d):

if n = 2 then the first product must have 4 digits and the second one 5 digits, which implies that d is in [5000, 9999]
if n = 3 then all 3 products must have 3 digits (we can rule out other length sequences because no such d could generate them), so
    d must be in [100, 333]
if n = 4 then the product sequence must have lengths [2, 2, 2, 3], which means d must be in [25, 33] (we can rule out all other possible length sequences on a
    case-by-case basis)
if n = 5 only [1, 2, 2, 2, 2] works, so d must be in [5..19]
if n = 6 only [1, 1, 1, 2, 2, 2] works, so d must be in [3]
if n = 7 only a length sequence of [1, 1, 1, 1, 1, 2, 2] will work.  No d can satisfy this sequence's requirements.
if n = 8 only a length sequence of [1, 1, 1, 1, 1, 1, 1, 2] would work.  No d can satisfy this sequence's requirements.

This means we will only have to check 5000 + 234 + 9 + 15 + 1 = 5259 concatenated products for the pandigital property

I came up with a better way of computing the pandigital property than what I used in the past, but for some reason I only have 32 bit
integers available on this machine despite being a 64 bit install, and so whatever performance I gained is likely lost due to having to use
Integer over Int

-}

import Data.Array
import Data.Array.ST

build_digital_signature_array :: Array Integer Integer
build_digital_signature_array =
    runSTArray( do
        {
            a <- newArray (0, 9) 0;
            writeArray a 0 2;
            writeArray a 1 3;
            writeArray a 2 5;
            writeArray a 3 7;
            writeArray a 4 11;
            writeArray a 5 13;
            writeArray a 6 17;
            writeArray a 7 19;
            writeArray a 8 23;
            writeArray a 9 29;
            return a
        })

has_digital_signature_aux :: Integer -> Array Integer Integer -> Integer -> Integer -> Bool
has_digital_signature_aux n dsa s acc
    | n == 0 = acc == s
    | otherwise = has_digital_signature_aux (div n 10) dsa s $ acc * (dsa ! (mod n 10))

has_digital_signature :: Integer -> Array Integer Integer -> Integer -> Bool
has_digital_signature n dsa s = has_digital_signature_aux n dsa s 1

pandigital_signature :: Integer
pandigital_signature = 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29

get_possible_generator_list :: Integer -> [Integer]
get_possible_generator_list 2 = [5000..9999]
get_possible_generator_list 3 = [100..333]
get_possible_generator_list 4 = [23..33]
get_possible_generator_list 5 = [5..19]
get_possible_generator_list 6 = [3]
get_possible_generator_list _ = []

build_product_list :: [Integer] -> Integer -> [Integer]
build_product_list l d =
    map (*d) l

-- uses the fact that we know the individual products are 5 digits or fewer
concatenate_product_list :: [Integer] -> Integer -> Integer
concatenate_product_list [] prod = prod
concatenate_product_list (x : xs) prod
    | x < 10 = concatenate_product_list xs $ prod * 10 + x
    | x < 100 = concatenate_product_list xs $ prod * 100 + x
    | x < 1000 = concatenate_product_list xs $ prod * 1000 + x
    | x < 10000 = concatenate_product_list xs $ prod * 10000 + x
    | otherwise = concatenate_product_list xs $ prod * 100000 + x

build_pandigital_list :: [Integer]
build_pandigital_list =
    filter (\x -> has_digital_signature x build_digital_signature_array pandigital_signature)
        [concatenate_product_list (build_product_list [1..n] d) 0 | n <- [2..6] :: [Integer], d <- get_possible_generator_list n ]

max_pandigital :: Integer
max_pandigital = foldl1 max build_pandigital_list

