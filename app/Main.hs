module Main where

import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import qualified Euler                         as E
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )


lookupProblem args = fromMaybe (last E.problems) $ do
  arg1 <- listToMaybe args
  num  <- readMaybe arg1
  E.lookup num E.problems

main :: IO ()
main = do
  lookupProblem <$> getArgs >>= E.showProblem
