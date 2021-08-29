{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.String.Interpolate
import           Euler                         as E
import           Test.Hspec

problemTest problem = it [i|##{problem ^. number}|] $ maybe
  (pendingWith "unsolved")
  (`shouldBe` problem ^. solution)
  (problem ^. reference)


main :: IO ()
main = hspec $ do
  mapM_ problemTest problems
