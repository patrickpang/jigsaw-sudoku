-- Modified from https://hub.darcs.net/thielema/set-cover/browse/example/Sudoku.hs

module Solver where

import Game
import SetCover

import Data.Array
import Data.Maybe
import qualified Math.SetCover.Exact as ESC

solveGame :: Game -> Board
solveGame game =
   let
      initAssigns =  assigns $ blocks game
      occupied = filter (isJust . snd) $ assocs $ board game
      occupiedAssigns = [assign n r c b | ((r, c), (Just n)) <- occupied, let b = (blocks game) ! (r, c)]
      solution = head $ ESC.search $ foldl (flip ESC.updateState) (ESC.initState initAssigns) occupiedAssigns
   in
      array ((0, 0), (8, 8)) [((r, c), (Just n)) | ((r, c), n) <- solution]