{-# LANGUAGE CPP, TemplateHaskell #-}
-- Welcome to Leksah.  This is a quick sample package for you to
-- try things out with.  We hope it will be useful for those new
-- to Haskell or just new to Leksah.

-- If you are new to haskell then here are some great sites to visit
-- http://learnyouahaskell.com/
-- http://tryhaskell.org/
-- http://book.realworldhaskell.org/

-- To build this package use
--   * Just make a change while background build is activated
--   * Ctrl+B (OS X Command+B)
--   * Package -> Build

-- When you are ready to create your own workspace and package.
--   * Package -> New
--   * When asked for a root folder for your package select a new folder
--     with the desired name of your package

-- This is the "Main" module and it exports a "main" function
module Main (
    main
) where

import Data.Array

factorial_aux n p
    | n <= 1 = p
    | otherwise = factorial_aux (n - 1) (n * p)

factorial n =
    factorial_aux n 1

max_check_bound n =
    let nine_factorial = factorial 9
    in if (n * nine_factorial) < 10 ^ n then 10 ^ n - 1 else max_check_bound (n + 1)

make_array f bounds = array bounds [(i, f i) | i <- range bounds]

fac_array = make_array factorial (0, 9)

gen_fac_sum_array n = a where a = array (0, n) [(i, if i < 10 then fac_array!i else (fac_array!(mod i 10) + a!(div i 10))) | i <- [0..n]]

fac_sum_array = gen_fac_sum_array (max_check_bound 1)

factorial_sum_property_list_v2 = [n | n <- [3..(max_check_bound 1)], n == fac_sum_array!n]

prob34_answer_v2 = sum factorial_sum_property_list_v2

--
-- To run it
--   * Select Leksah menu item Package -> Run (or the cogs on the toolbar)
--   * Select "exeMain" and press Ctrl+Enter to run them in ghci
--   * Run "leksah-wellcome" from the command line
main = do
    putStrLn "test"
    putStrLn (show (fac_array!9))
    putStrLn (show prob34_answer_v2)



