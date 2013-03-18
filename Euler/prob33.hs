-- Project Euler, Problem 33 - Digit Cancelling Fractions
--
-- My gripe with this problem is that "trivial" is very poorly defined (by a single example with no explanation, cmon)
--
-- There are several ways of interpreting the trivial example:
--  1) A fraction is trivial if the cancellation is done in the same digit index
--  2) A fraction is trivial if both numbers end in 0
--  3) A fraction is trivial if the cancellation results in the lowest term fraction
--
-- For two digit numbers, 1 and 2 are equivalent.
-- I'm going to start off by assuming this (1, 2) definition of trivial.
--
-- Ultimately, since we're given that there are only 4 non-trivial instances, I'll settle on whichever definition gives me that

lowest_terms (n, d) =
    let divisor = gcd n d in
        (div n divisor, div d divisor)

cancel_digits_12 (n, d) =
    let n1 = mod n 10
        n10 = div n 10
        d1 = mod d 10
        d10 = div d 10
    in if n1 == d10 then (n10, d1) else (n, d)

cancel_digits_21 (n, d) =
    let n1 = mod n 10
        n10 = div n 10
        d1 = mod d 10
        d10 = div d 10
    in if n10 == d1 then (n1, d10) else (n, d)


fractions_to_check = [ (n, d) | d <- [11..99], n <-  [10..(d - 1)]]

cancellable_fractions_12 = [ f | f@(n, d) <- fractions_to_check, let f1@(n1, d1) = cancel_digits_12 f, f /= f1, lowest_terms f == lowest_terms f1]

cancellable_fractions_21 = [ f | f@(n, d) <- fractions_to_check, let f1@(n1, d1) = cancel_digits_21 f, f /= f1, lowest_terms f == lowest_terms f1]

cancellable_fractions = cancellable_fractions_12 ++ cancellable_fractions_21

fold_accum (n1, d1) (n2, d2) =
    (n1 * n2, d1 * d2)

final_fraction = lowest_terms (foldl1 fold_accum cancellable_fractions)





