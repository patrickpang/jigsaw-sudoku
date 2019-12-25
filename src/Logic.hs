module Logic where

import Model

import Data.List
import Data.Maybe
import Data.Function

makeMove :: Game -> Coord -> Cell -> Game
makeMove game (r, c) cell =
  let 
    newBoard = 
      [
        [ 
          if (r == r') && (c == c') then cell else cell' 
          | (c', cell') <- zip [0..] row
        ] 
        | (r', row) <- zip [0..] (board game)
      ]
  in
    game{board = newBoard}
  -- assumption: valid number (from UI)
  -- replace both empty and occupied cell
  -- no need to check conflicts, which will be shown by UI
  -- TODO: initial cells -> return same board

allConflicts :: Game -> [Coord]
allConflicts Game{board=board, config=config, blocks=blocks} =
  union blockConflicts $ union rowConflicts columnConflicts
  where
    rowConflicts = conflictsOn (\((r, _), _) -> r) board
    columnConflicts = conflictsOn (\((_, c), _) -> c) board
    blockConflicts = conflictsOn (\((r, c), _) -> config !! r !! c) board

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
    
    groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
    groupSortOn f xs = groupBy ((==) `on` f) $ sortOn f xs

    assocs :: Board -> [(Coord, Cell)]
    assocs board =
      concat [[((r, c), cell) | (c, cell) <- zip [0..] row] | (r, row) <- zip [0..] board]
  
    -- find conflicting cells globally
    -- for highlight of errors before rendering

-- isEnded
-- no Nothing & no conflicts