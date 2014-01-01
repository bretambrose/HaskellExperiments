{-
    Problem 44 - Pentagon numbers

    See problem 45 for an explanation of pentagonal number testing.

    I tried playing with sums and differences algebraically, but didn't really get anywhere.

    So I looked at the different series produced by pentagonal differences of increasing gaps:

    diff1 = (5-1), (12-5), (22-12), (35-22), (51-35), etc...
          = 4, 7, 10, 13, 16, ...
          = 1 + 3n, for n >= 1

    diff2 = (12-1), (22-5), (35-12), (51-22), etc...
          = 11, 17, 23, 29, etc...
          = 5 + 6n, for n >= 1

    diff3 = 12 + 9n, for n >= 1

    and generally,

    diff_i = P_i + 3 * i * n, for n >= 1, where P_i is the ith Pentagonal number

    We'll solve the problem by iterating through potential differences (which must be Pentagonal,
    hence we'll be iterating the Pentagonal numbers) and using this formula to generate all
    possible pairs that could yield the number as a difference, and check whether their
    sum is also pentagonal.  To support debugging, I ended up also returning both the
    difference and the first pentagonal number in the pair.

-}

isPentagonalNumber :: Int -> Bool
isPentagonalNumber n
    | n <= 0 = False
    | otherwise =
        let discriminant = 1 + 24 * n
            expression = (round . sqrt $ fromIntegral discriminant) in
                expression ^ 2 == discriminant && mod (1 + expression) 6 == 0

genPentagonalNumber :: Int -> Int
genPentagonalNumber n = div (n * (3 * n - 1 ) ) 2


findSmallestPentagonalDifferenceAux2 :: Int -> [(Int, Int)] -> Maybe Int
findSmallestPentagonalDifferenceAux2 n ((i, p):xs) =
    if p < n then
        let rem = mod (n - p) (3 * i)
            n_i = div (n - p) (3 * i)
            p_1 = genPentagonalNumber n_i
            p_2 = p_1 + n in
                if rem == 0 && n_i > 0 && isPentagonalNumber (p_1 + p_2) then
                    Just p_1
                else
                    findSmallestPentagonalDifferenceAux2 n xs
    else
        Nothing


findSmallestPentagonalDifferenceAux :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int)
findSmallestPentagonalDifferenceAux ((i, p):xs) l =
    case findSmallestPentagonalDifferenceAux2 p l of
        Nothing -> findSmallestPentagonalDifferenceAux xs l
        Just n -> (p, n)



findSmallestPentagonalDifference :: (Int, Int)
findSmallestPentagonalDifference =
    let pentagonalList = [ (n, genPentagonalNumber n) | n <- [1..] ] in
        findSmallestPentagonalDifferenceAux pentagonalList pentagonalList

