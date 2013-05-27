-- Project Euler, Problem 39 - Integer right triangles

{-

This problem has similarities to problem 9, special pythagoran triplet and in fact the analysis is exactly
the same.  See EulerPublic/Euler/FSharpSolutions/problem9.fs

The fact that a <= b rather than a < b in this problem is irrelevant.  If a = b then
c^2 = 2*b^2, which implies that c is not an integer, so no isosceles solutions exist.

Wlog, we'll let a be the smallest of the two legs, so a <= b < c

For each p, starting with p = 3 (although we can start higher, namely 12)

b can range from (sqrt( p^2 - 9 ) / 3) to ((p + 1)/2 - 1)

For a fixed b, we can solve for the only a that might satisfy the Pythagorean formula:

a = ( p * ( 2b - p ) ) / ( 2 * ( b - p ) )

-}

triple_filter :: (Int, Int, Int) -> Bool
triple_filter (a, b, c) =
    ( a <= b ) && ( b < c ) && ( a > 0 ) && ( a * a + b * b == c * c )

generate_triple_list :: Int -> [(Int, Int, Int)]
generate_triple_list p =
    let min_b = floor (sqrt( fromIntegral ( p * p - 9 )) / 3)
        max_b = (div (p + 1) 2) - 1 in
            filter triple_filter [ (a, b, p - a - b) | b <- [min_b..max_b], a <- [div (p * (2 * b - p)) (2 * (b - p))] ]

generate_triple_pair_list :: Int -> [(Int, Int)]
generate_triple_pair_list n
    | n < 12 = []
    | otherwise = [ (d, length $ generate_triple_list d) | d <- [12..n] ]

max_triple_pair :: (Int, Int) -> (Int, Int) -> (Int, Int)
max_triple_pair (ix, x) (iy, y)
    | x < y = (iy, y)
    | otherwise = (ix, x)

max_pythagorean_triples :: Int -> Int
max_pythagorean_triples n
    | n < 12 = 0
    | otherwise =
        let (x, y) = foldl max_triple_pair (0, 0) $ generate_triple_pair_list n in
            x


