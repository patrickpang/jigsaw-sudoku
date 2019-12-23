module Main where
  
import Serialization
import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
  (screenWidth, screenHeight) <- getScreenSize
  let 
    windowWidth = 600
    windowHeight = 400
    windowLeft = (screenWidth - windowWidth) `div` 2
    windowTop = (screenHeight - windowHeight) `div` 2
    window = InWindow "Jigsaw Sudoku" (windowWidth, windowHeight) (windowLeft, windowTop)

  game <- loadGame "../map.txt"
  play window white 100 game renderWorld handleEvent updateWorld

renderWorld :: Game -> Picture
renderWorld game = Scale 0.1 0.1 $ Text $ show game

handleEvent :: Event -> Game -> Game
handleEvent _ game = game

updateWorld :: Float -> Game -> Game
updateWorld _ game = game
