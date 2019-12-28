{-# LANGUAGE NamedFieldPuns #-}

module Logic where

import Game
import Utils (groupSortOn)

import Data.List
import Data.Array
import Data.Maybe

makeMove :: Game -> Board -> Coord -> Cell -> Game
makeMove game initial coord cell =
  if isNothing $ initial ! coord 
  then game{board = (board game) // [(coord, cell)]} 
  else game
  -- assumption: valid number (from UI)
  -- replace both empty and occupied cell
  -- no need to check conflicts, which will be shown by UI
  -- TODO: initial cells -> return same board

allConflicts :: Game -> [Coord]
allConflicts Game{board, blocks} =
  union blockConflicts $ union rowConflicts columnConflicts
  where
    rowConflicts = conflictsOn (\((r, _), _) -> r) board
    columnConflicts = conflictsOn (\((_, c), _) -> c) board
    blockConflicts = conflictsOn (\((r, c), _) -> blocks ! (r, c)) board

    conflictsOn :: Ord a => ((Coord, Cell) -> a) -> Board -> [Coord] 
    conflictsOn f board = 
      concatMap conflicts $ groupSortOn f $ assocs board

    conflicts :: [(Coord, Cell)] -> [Coord]
    conflicts cells = 
      let
        occupied = filter (isJust . snd) cells
        groups = groupSortOn snd $ occupied
      in
        concat [map fst group | group <- groups, length group > 1]
  
    -- find conflicting cells globally
    -- for highlight of errors before rendering

isEnded :: Game -> Bool
isEnded game =
  (all isJust $ elems $ board game) && (null $ allConflicts game)
  -- no Nothing & no conflicts