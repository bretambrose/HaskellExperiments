-- Project Euler, Problem 34 - Digit Factorials
--
-- There are two key components to this problem: (1) efficiently testing a given number for the factorial sum property and (2) coming up with
-- a reasonably accurate upper bound for how many numbers need to be checked (ie, find a reasonable N such that all numbers greater than N
-- cannot possibly have the property).
--
-- For (1), I have no idea if this will work, but I'm going to develop a function that, given pureness, a lot of recomputation can be avoided through
-- memoization.  It's my hope that Haskell will, by default, memoize things.  If not, oh well.
--
-- For (2), the key is finding N such that N * 9! < 10 ^ N.  Knowing that, you need to check all numbers from 3 (well you can start much later
-- just by eyeballing, but for simplicity we'll start at 3) to 10 ^ N - 1.

-- basic tail recursive factorial
import Data.Array

factorial_aux n p
    | n <= 1 = p
    | otherwise = factorial_aux (n - 1) (n * p)

factorial n =
    factorial_aux n 1


-- hoping this gets memoized; note that this is not tail recursive by design: the recursion depth is low even in the worst case and using
-- tail recursion would make memoization impossible
factorial_sum n
    | n < 10 = factorial n
    | otherwise = (factorial (mod n 10)) + (factorial_sum (div n 10))

-- weird to write an "increasing" recursive function
max_check_bound n =
    let nine_factorial = factorial 9
    in if (n * nine_factorial) < 10 ^ n then 10 ^ n - 1 else max_check_bound (n + 1)

factorial_sum_property_list = [ n | n <- [3..(max_check_bound 1)], n == factorial_sum n]

prob34_answer = sum factorial_sum_property_list


--
-- Ok, the performance of that was unacceptable.  Further checking showed no memoization gets automatically done, and I'll need
-- to do it myself.
-- I used http://www.haskell.org/tutorial/arrays.html as a model for this

make_array f bounds = array bounds [(i, f i) | i <- range bounds]

fac_array = make_array factorial (0, 9)

gen_fac_sum_array n = a where a = array (0, n) [(i, if i < 10 then fac_array!i else (fac_array!(mod i 10) + a!(div i 10))) | i <- [0..n]]

fac_sum_array = gen_fac_sum_array (max_check_bound 1)

factorial_sum_property_list_v2 = [n | n <- [3..(max_check_bound 1)], n == fac_sum_array!n]

prob34_answer_v2 = sum factorial_sum_property_list_v2

-- even then, it still takes approx 10-15 seconds to compute which is mind-boggling.  calculating the value of a single element is two
-- array lookups and an addition.  While the array is large (10 million) elements, an imperative solution would take < a second

-- even after converting to an executable and compiling with -O, it's still a 3-4 second wait



