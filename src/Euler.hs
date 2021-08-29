{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Euler where

import           Control.Lens
import           Control.Monad                  ( guard )
import           Data.Function                  ( (&) )
import           Data.List                      ( find
                                                , sort
                                                )
import           Data.String.Interpolate

orF f g x = f x || g x

divisibleBy div num = num `mod` div == 0

problem1 :: Int
problem1 = [1 .. 999] & filter (orF (divisibleBy 3) (divisibleBy 5)) & sum


fibonacci n1 n2 = n1 : (fibonacci n2 $ n1 + n2)

problem2 = fibonacci 1 2 & tail & filter even & takeWhile (<= 4000000) & sum


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


palindrome list = list == reverse list

problem4 = head $ reverse $ sort $ do
  let nums = [999, 998 .. 100]
  num1 <- nums
  num2 <- nums
  let product = num1 * num2
  guard $ palindrome $ show product
  return product


problem5 = foldl1 lcm [1 .. 20]


pandigital digits = sort digits == "123456789"

pandigitalBothEnds digits =
  (pandigital $ take 9 $ digits) && (pandigital $ take 9 $ reverse digits)

problem104 =
  fst
    $ head
    $ filter (pandigitalBothEnds . snd)
    $ zip [1 ..]
    $ map show
    $ fibonacci 1 1



data Problem = Problem
  { _number    :: Int
  , _reference :: Maybe String
  , _solution  :: String
  }

$(makeLenses ''Problem)

makeSolved :: (Show a) => Int -> a -> a -> Problem
makeSolved num reference solution =
  Problem num (Just $ show reference) (show solution)

makeUnsolved :: (Show a) => Int -> a -> Problem
makeUnsolved num solution = Problem num Nothing (show solution)


lookup :: Int -> [Problem] -> Maybe Problem
lookup num problems = find ((==) num . _number) problems

showProblem (Problem num mref solution) = do
  case mref of
    Just reference -> putStrLn $ [i|##{num}
reference : #{reference}
solution  : #{solution}|]

    Nothing -> putStrLn $ [i|##{num}
solution  : #{solution}|]


problems :: [Problem]
problems =
  [ makeSolved 1   233168    problem1
  , makeSolved 2   4613732   problem2
  , makeSolved 3   6857      problem3
  , makeSolved 4   906609    problem4
  , makeSolved 5   232792560 problem5
  , makeSolved 104 329468    problem104
  ]
