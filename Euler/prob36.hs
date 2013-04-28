-- Project Euler, Problem 36 - Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2
--
-- For the palindrome checks, I figured doing things with pure integer arithmetic would be most efficient.  With that in mind,
-- the only trouble was computing the high-digit mask/selector.  Knowing that we're processing the numbers in simple 1..n order,
-- the high-digit selector remains the same or increases by a factor of 2/10 and we use a fold to keep track of both of these
-- values while processing the list.

import Data.Bits

is_binary_palindrome :: Int -> Int -> Int -> Bool
is_binary_palindrome n h l
    | h <= l = True
    | otherwise =
        let high_digit = if n .&. h /= 0 then 1 else 0
            low_digit = if n .&. l /= 0 then 1 else 0 in
                if high_digit /= low_digit then
                    False
                else
                    is_binary_palindrome n (shiftR h 1) (shiftL l 1)


is_decimal_palindrome :: Int -> Int -> Bool
is_decimal_palindrome n m
    | m <= 1 = True
    | otherwise =
        let high_digit = (div n m)
            low_digit = mod n 10 in
                if high_digit /= low_digit then
                    False
                else
                    is_decimal_palindrome (div (mod n m) 10) (div m 100)


folder :: (Int, Int, Int) -> Int -> (Int, Int, Int)
folder (pal_sum, h10, h2) val =
    let next_val = val + 1
        next_h10 = if next_val == h10 * 10 then h10 * 10 else h10
        next_h2 = if (next_val .&. val) == 0 then h2 * 2 else h2 in
        if (is_binary_palindrome val h2 1) && (is_decimal_palindrome val h10) then
            (val + pal_sum, next_h10, next_h2)
        else
            (pal_sum, next_h10, next_h2)

double_palindrome_sum n
    | n < 1 = 0
    | otherwise =
        let (pal_sum, _, _) = foldl folder (0, 1, 1) [1..n] in
            pal_sum


