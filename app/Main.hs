{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Brick                         as B
import qualified Brick.Widgets.Border          as BB
import qualified Brick.Widgets.Border.Style    as BB
import qualified Brick.Widgets.List            as BL
import           Control.Lens
import           Data.Foldable
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                , listToMaybe
                                                )
import           Data.String.Interpolate
import qualified Data.Vector                   as Vec
import qualified Euler                         as E
import qualified Graphics.Vty                  as V
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )

lookupProblem args = fromMaybe (last E.problems) $ do
  arg1 <- listToMaybe args
  num  <- readMaybe arg1
  E.lookup num E.problems

doCommandLine = lookupProblem <$> getArgs >>= E.showProblem


{-- TUI --}

data State = State
  { _problems :: BL.GenericList () Vec.Vector E.Problem
  }

$(makeLenses ''State)

drawProblem selected (E.Problem num mref _) =
  B.str [i| ##{num} #{fromMaybe "?" mref}|]

draw :: State -> [B.Widget ()]
draw state =
  [ B.withBorderStyle BB.unicode
      $  BB.border
      $  B.padLeftRight 1
      $  BL.renderList drawProblem True
      $  state
      ^. problems
  ]


update :: State -> B.BrickEvent () e -> B.EventM () (B.Next State)
update state (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt state
update state (B.VtyEvent event) =
  B.handleEventLensed state problems BL.handleListEvent event >>= B.continue


attributes = const $ B.attrMap
  V.defAttr
  [(BL.listSelectedAttr, V.defAttr `V.withStyle` V.reverseVideo)]

eulerApp = B.App { B.appDraw         = draw
                 , B.appChooseCursor = B.showFirstCursor
                 , B.appHandleEvent  = update
                 , B.appStartEvent   = return
                 , B.appAttrMap      = attributes
                 }


state0 = State $ BL.listFindBy (isNothing . view E.reference) $ BL.list
  ()
  (Vec.fromList E.problems)
  1

main :: IO ()
main = do
  B.defaultMain eulerApp state0
  return ()
