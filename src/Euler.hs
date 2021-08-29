module Euler
  ( problems
  ) where

import           Data.Function                  ( (&) )

orF f g x = f x || g x

divisibleBy div num = num `mod` div == 0

problem1 :: Int
problem1 = [1 .. 999] & filter (orF (divisibleBy 3) (divisibleBy 5)) & sum


getFibonacci n1 n2 = n1 : (getFibonacci n2 $ n1 + n2)

fibonacci = getFibonacci 1 2

problem2 = fibonacci & filter even & takeWhile (<= 4000000) & sum


divides = flip divisibleBy

getPrimes knownPrimes@(lastPrime : others) =
  lastPrime : (getPrimes $ (tryPrime $ lastPrime + 2) : knownPrimes)
 where
  tryPrime num =
    if any (divides num) knownPrimes then tryPrime $ num + 2 else num

primes = 2 : (getPrimes [3, 2])

factors num = loop num [] primes
 where
  loop 1   acc _                    = acc
  loop num acc (nextPrime : others) = case quotRem num nextPrime of
    (rest, 0) -> loop rest (nextPrime : acc) (nextPrime : others)
    _         -> loop num acc others

problem3 = head $ factors 600851475143


problems :: [(Int, String)]
problems = [(1, show $ problem1), (2, show $ problem2), (3, show $ problem3)]
