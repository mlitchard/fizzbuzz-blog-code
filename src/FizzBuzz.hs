{-# LANGUAGE MonadComprehensions #-}

module FizzBuzz 
    ( fizzbuzz
    , fib_con
    ) where

import Data.Semigroup ((<>),getOption)
import Data.Maybe (fromMaybe)

import Data.Numbers.Primes (isPrime)

-- This is the code from part-2 of Haskell As Engineering Language
-- https://github.com/mlitchard/fizzbuzz-blog-code/tree/part-2
-- Entirely cribbed from 
-- http://dave.fayr.am/posts/2012-10-4-finding-fizzbuzz.html
-- Discussion can be found on reddit
-- https://www.reddit.com/r/haskell/comments/10zlyy/fizzbuzz_revisited_using_monoids/
fizzbuzz :: Integer -> String
fizzbuzz i = fromMaybe (show i) $ getOption fizzbuzz'
  where
    fizzbuzz' =
      ["fizz "        | i `rem` 3 == 0] <>
      ["buzz "        | i `rem` 5 == 0] <>
      ["bang!"        | isPrime i     ]

-- Binet's formula
-- https://wiki.haskell.org/The_Fibonacci_sequence#Using_Binet.27s_formula
fib_con :: Integer -> Integer
fib_con n = (round $ phi ** fromIntegral n / sq5)
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2
