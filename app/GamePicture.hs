{-# LANGUAGE NamedFieldPuns #-}

module GamePicture where

import Game

import Data.Array
import Data.Maybe
import Graphics.Gloss

cellLength :: Float
cellLength = 40.0

renderBoard :: Game -> Board -> [Coord] -> Picture
renderBoard game initial conflicts = pictures
  [
    translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $ 
      renderCell cell block hasConflicts isInitial
    | ((r, c), cell) <- assocs $ board game,
    let block = (blocks game) ! (r, c), 
    let hasConflicts = elem (r, c) conflicts,
    let isInitial = isJust $ initial ! (r, c)
  ]

renderCell :: Cell -> Int -> Bool -> Bool -> Picture
renderCell cell block hasConflicts isInitial = pictures 
  [
    color (colorsOfBlocks !! block) $ rectangleSolid cellLength cellLength,
    color white $ rectangleWire cellLength cellLength,
    color textColor $ translate (- cellLength / 4) (- cellLength / 4) $ 
      scale 0.2 0.2 $ text $ maybe "" show cell
  ]
  where
    textColor = 
      if hasConflicts then red
      else if isInitial then black
      else greyN 0.5

renderFocus :: Coord -> Picture
renderFocus (r, c) =
  color black $ translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $
    rectangleWire cellLength cellLength

renderStatus :: [Coord] -> Bool -> Int -> Picture
renderStatus conflicts ended steps =
  translate (-180) (-200) $ scale 0.1 0.1 $ text $
  if ended then "Congratulations! You've finished in " ++ show steps ++ " steps!" 
  else "No. of conflicts: " ++ show (length conflicts)

colorsOfBlocks :: [Color]
colorsOfBlocks = map (withAlpha 0.5)
  [
    green,
    red,
    aquamarine,
    yellow,
    cyan,
    magenta,
    rose,
    chartreuse,
    azure
  ]