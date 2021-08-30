{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Euler where

import           Control.Lens
import           Control.Monad                  ( guard )
import           Data.Char                      ( digitToInt )
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


square x = x * x

problem6 = square (sum [1 .. 100]) - sum (map square [1 .. 100])


problem7 = primes !! 10000


problem8string =
  "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

problem8digits = map digitToInt problem8string

problem8 = loop problem8digits 0
 where
  loop digits@(a : b : c : d : e : f : g : h : i : j : k : l : m : rest) biggest
    = loop (tail digits)
           (max biggest $ a * b * c * d * e * f * g * h * i * j * k * l * m)
  loop (b : c : d : e : f : g : h : i : j : k : l : m : []) biggest = biggest


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
  [ makeSolved 1   233168      problem1
  , makeSolved 2   4613732     problem2
  , makeSolved 3   6857        problem3
  , makeSolved 4   906609      problem4
  , makeSolved 5   232792560   problem5
  , makeSolved 6   25164150    problem6
  , makeSolved 7   104743      problem7
  , makeSolved 8   23514624000 problem8
  , makeSolved 104 329468      problem104
  ]
