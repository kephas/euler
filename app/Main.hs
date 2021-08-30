{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Brick                          ( (<+>) )
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

data RenderedProblem
  = UnevaluatedWithReference { num :: Int, ref :: String, solution :: String }
  | GoodSolution { num :: Int, solution :: String }
  | BadSolution { num :: Int, ref :: String, solution :: String }
  | UnevaluatedWithoutReference { num :: Int, solution :: String }
  | EvaluatedWithoutReference { num :: Int, solution :: String }

data State = State
  { _problems :: BL.GenericList () Vec.Vector RenderedProblem
  }

$(makeLenses ''State)


renderProblem (E.Problem num mref solution) = case mref of
  Just reference -> UnevaluatedWithReference num reference solution
  Nothing        -> UnevaluatedWithoutReference num solution

evaluateProblem problem = case problem of
  UnevaluatedWithReference num ref solution -> if ref == solution
    then GoodSolution num solution
    else BadSolution num ref solution

  UnevaluatedWithoutReference num solution ->
    EvaluatedWithoutReference num solution

  _ -> problem

isUnevaluatedWithoutReference p = case p of
  UnevaluatedWithoutReference _ _ -> True
  _                               -> False


orange = V.Color240 192

withTextColor color = B.modifyDefAttr (flip V.withForeColor color)

drawProblem _ problem = case problem of
  UnevaluatedWithReference num ref _ ->
    B.modifyDefAttr (flip V.withForeColor orange) $ B.str [i|##{num} #{ref}|]

  GoodSolution num solution ->
    withTextColor V.green $ B.str [i|##{num} #{solution}|]

  BadSolution num ref solution ->
    withTextColor V.red
      $   B.str [i|##{num} |]
      <+> (withTextColor V.green $ B.str ref)
      <+> B.str [i| != #{solution}|]

  UnevaluatedWithoutReference num _        -> B.str [i|##{num} ?|]
  EvaluatedWithoutReference   num solution -> B.str [i|##{num} #{solution}|]

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
update state (B.VtyEvent (V.EvKey V.KEnter [])) =
  B.continue (state & problems %~ BL.listModify evaluateProblem)
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


state0 = State $ BL.listFindBy isUnevaluatedWithoutReference $ BL.list
  ()
  (Vec.fromList $ map renderProblem E.problems)
  1

main :: IO ()
main = do
  B.defaultMain eulerApp state0
  return ()
