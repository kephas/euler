module Euler
  ( problems
  ) where

import           Data.Function                  ( (&) )

orF f g x = f x || g x

divisibleBy div num = num `mod` div == 0

--sum = foldr (+) 0

problem1 :: Int
problem1 = [1 .. 999] & filter (orF (divisibleBy 3) (divisibleBy 5)) & sum


fibonacci n1 n2 = n1 : (fibonacci n2 $ n1 + n2)

problem2 = fibonacci 1 2 & filter even & takeWhile (<= 4000000) & sum


problems :: [(Int, String)]
problems = [(1, show $ problem1), (2, show $ problem2)]
