-- Project Euler, Problem 37 - truncatable primes
{-
I spent a lot of time thinking about this problem.  My final solution is definitely not elegant, but by transforming the
problem from search-based to a positive construction, it becomes much, much more efficient and it solves the mystery of
why there's only 12 such numbers.

Let D be an n-digit number, and d_i be the ith digit, so d_i > 0 for all i /= n

By definition, D is left-truncatable prime iff D is prime and D' = d_1 d_2 ... d_(n-1) is left-truncatable prime

similarly

D is right-truncatable prime iff D is prime and D' = d_2 d_3 .. d_n is right-truncatable prime

This implies we can positively construct the n-digit left/right truncatable primes from a list
of the (n-1) digit left/right truncatable primes

By adding in some basic arithmetic reasoning, we can heavily decrease the amount of numbers
we need to check while doing this construction.

In particular, for D to be left-truncatable prime,
    d_1 must be in {2, 3, 5, 7}
    d_i must be in {1, 3, 7, 9} where i > 1

For D to be a right-truncatable prime,
    d_n must be in {3, 7}

Since we're looking for primes that are both left and right truncatable, we can also use some of the left-truncatable
restrictions when generating the right-truncatable ones.

This leads us to two functions, build_left_trunc and build_right_trunc, that enumerate left and right truncatable primes.
Note that these two functions do not enumerate all left and right truncatable primes; they enumerate a subset that
could possibly be both.

The mystery of why there's only 12 such numbers follows from the fact that the left-truncatable primes list "ends" because
with a positive construction, once there's an N such that no N-digit left-trunctable primes exist, there can be no more period and
thus no more trunctable primes either.

-}

is_prime_aux :: Int -> Int -> Int -> Bool
is_prime_aux n d max_d =
    if d > max_d then
        True
    else if (mod n d) == 0 then
        False
    else
        is_prime_aux n (d + 2) max_d


is_prime :: Int -> Bool
is_prime n
    | n <= 1 = False
    | otherwise = (mod n 2) == 1 && is_prime_aux n 3 (round $ sqrt $ fromIntegral n)


build_left_trunc_aux :: Int -> Int -> [Int] -> [Int] -> [Int]
build_left_trunc_aux i n l p
    | i == 1 = build_left_trunc_aux (i + 1) n [2, 3, 5, 7] []
    | (i > n) || null l = p
    | otherwise =
        let cl = [ 10 * d + j | d <- l, j <- [1, 3, 7, 9], is_prime $ 10 * d + j]
            pl = [ d | d <- cl, (mod d 10) == 3 || (mod d 10) == 7] in
            build_left_trunc_aux (i + 1) n cl $ p ++ pl

build_left_trunc :: Int -> [Int]
build_left_trunc n
    | n < 1 = []
    | otherwise = build_left_trunc_aux 1 n [] []


build_right_trunc_aux :: Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
build_right_trunc_aux i n l p
    | i == 1 = build_right_trunc_aux (i + 1) n [3, 7] []
    | (i > n) || null l = (l, p)
    | otherwise =
        let high = round $ 10 ^ (i - 1)
            high2 = round $ 10 ^ (i - 2)
            cl = [ high * f + d | f <- [1,2,3,5,7,9], d <- l, (div d high2) /= 2 && (div d high2) /= 5, is_prime $ high * f + d] in
                build_right_trunc_aux (i + 1) n cl $ p ++ cl

build_right_trunc :: Int -> [Int]
build_right_trunc n
    | n < 1 = []
    | otherwise = snd $ build_right_trunc_aux 1 n [] []

trunc_prime_list_aux :: [Int] -> [Int] -> [Int] -> [Int]
trunc_prime_list_aux [] r p = p
trunc_prime_list_aux l [] p = p
trunc_prime_list_aux xp@(x:xs) yp@(y:ys) p
    | x < y = trunc_prime_list_aux xs yp p
    | x > y = trunc_prime_list_aux xp ys p
    | otherwise = trunc_prime_list_aux xs ys $ x : p

trunc_prime_list :: Int -> [Int]
trunc_prime_list n
    | n < 1 = []
    | otherwise =
        let ls = build_left_trunc n
            rs = build_right_trunc n in
                trunc_prime_list_aux ls rs []

