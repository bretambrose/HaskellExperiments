-- Project Euler, Problem 40 - Champernowne's constant

{-


-}

champ_aux1 :: Int -> Int -> Int -> (Int, Int, Int)
champ_aux1 n d b =
    if n <= 9 * b * d then
        (d, n - 1, b)
    else
        champ_aux1 (n - 9 * b * d) (d + 1) (b * 10)

champ1 :: Int -> (Int, Int, Int)
champ1 n =
    champ_aux1 n 1 1


champ2 :: (Int, Int, Int) -> (Int, Int)
champ2 (d, r, b) =
    (b + div r d, round (10 ^^ ( d - 1 - mod r d)))

champ :: Int -> Int
champ n =
    let (v, p) = champ2 $ champ1 n in
        mod (div v p) 10

champ_prod = champ 1 * champ 10 * champ 100 * champ 1000 * champ 10000 * champ 100000 * champ 1000000



