module Main where

import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Euler                          ( problems )
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ fromMaybe "" $ do
    arg1 <- listToMaybe args
    num  <- readMaybe arg1
    lookup num problems
