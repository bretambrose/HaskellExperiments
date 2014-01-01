{-
    Problem 45 - Triangular, pentagonal, hexagonal

    See problem 42 for an explanation of triangular number testing.

    Similar analysis leads to a corresponding test for pentagonal numbers:

    x(3*x - 1)/2 = n
    3 * x^2 - x - 2n = 0

    roots = (1 +- sqrt( 1 + 24n )) / 6

    So we're looking for an integral, positive solution to

    sqrt( 1 + 24 * n ) / 6

    so we want 1 + sqrt( 1 + 24 * n ) to be divisible by 6.

    With those two tests in hand, we simply enumerate hexagonal numbers and test
    for the triangular and pentagonal properties.

-}


isTriangularNumber :: Integer -> Bool
isTriangularNumber n
    | n <= 0 = False
    | otherwise =
        let discriminant = 1 + 8 * n in
            (round . sqrt $ fromIntegral discriminant) ^ 2 == discriminant

isPentagonalNumber :: Integer -> Bool
isPentagonalNumber n
    | n <= 0 = False
    | otherwise =
        let discriminant = 1 + 24 * n
            expression = (round . sqrt $ fromIntegral discriminant) in
                expression ^ 2 == discriminant && mod (1 + expression) 6 == 0

findNextSpecialNumber :: Integer -> Integer
findNextSpecialNumber n =
    let hexNum = n * ( 2 * n - 1 ) in
        if isTriangularNumber hexNum && isPentagonalNumber hexNum then
            hexNum
        else
            findNextSpecialNumber $ n + 1

prob45answer = findNextSpecialNumber 144

