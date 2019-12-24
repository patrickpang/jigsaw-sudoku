module Main where
  
import Serialization
import Model
import Colors

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
  (screenWidth, screenHeight) <- getScreenSize
  let 
    windowWidth = 360
    windowHeight = 360
    windowLeft = (screenWidth - windowWidth) `div` 2
    windowTop = (screenHeight - windowHeight) `div` 2
    window = InWindow "Jigsaw Sudoku" (windowWidth, windowHeight) (windowLeft, windowTop)

  game <- loadGame "../map.txt"
  play window white 100 game renderWorld handleEvent updateWorld

renderWorld :: Game -> Picture
renderWorld game = renderGame game

cellLength :: Float
cellLength = 40.0

renderGame :: Game -> Picture
renderGame Game{board=board, config=config} = pictures $ concat 
  [
    [
      translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $ 
        renderCell cell (colorsOfBlocks !! (config !! r !! c))
      | (c, cell) <- zip [0..] row
    ] 
    | (r, row) <- zip [0..] board
  ]

renderCell :: Cell -> Color -> Picture
renderCell cell blockColor = pictures 
  [
    color blockColor $ rectangleSolid cellLength cellLength,
    color white $ rectangleWire cellLength cellLength,
    color black $ translate (- cellLength / 4) (- cellLength / 4) $ scale 0.2 0.2 $ text $ maybe "" show cell
  ] 

handleEvent :: Event -> Game -> Game
handleEvent _ game = game

updateWorld :: Float -> Game -> Game
updateWorld _ game = game
