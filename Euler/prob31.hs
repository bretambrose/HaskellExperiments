-- Problem 31 - Project Euler
--
-- Returning to project euler because I'm learning Haskell
--
-- This problem is a straightforward dynamic programming situation, where you calculate the number of ways
-- you can make change with a variety of currencies though the following recurrence:
--
-- Let the currencies be a list/array: C = [1, 2, 5, 10, 20, 50, 100, 200]
--
-- Let D(n, i) = number of ways you can make change totalling n, using just coins C[0] through C[i]
--
-- Then
--
-- D(n, 0) = 1
-- since there is only one way to total n (n pennies)
--
-- For the general case, you calculate it by summing the number of ways you can make change
-- for n using between 0 and k coins of type i, which can be expressed with the recurrence
--
-- D(n, i) = Sum(k in [0..(n / C[i])], D( n - k * C[i], i - 1))
--
-- With that in place, the answer we're looking for is D(200, 7)
coinsum_aux n 0 = 1
coinsum_aux n i =
    let currency_values = [1, 2, 5, 10, 20, 50, 100, 200]
        value = (currency_values !! i)
    in sum [ coinsum_aux (n - j * value ) (i - 1) | j <- [0..(div n value)]]

problem31 n = coinsum_aux n 7
