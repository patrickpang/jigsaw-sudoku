module Main where
  
import Serialization
import Model
import Colors

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)

data State =
  Playing {game :: Game, focus :: Coord}

main :: IO ()
main = do
  (screenWidth, screenHeight) <- getScreenSize
  let 
    windowWidth = 360
    windowHeight = 360
    windowLeft = (screenWidth - windowWidth) `div` 2
    windowTop = (screenHeight - windowHeight) `div` 2
    window = InWindow "Jigsaw Sudoku" (windowWidth, windowHeight) (windowLeft, windowTop)

  game <- loadGame "maps/map.txt"
  let state = Playing{game=game, focus=(0,0)}

  play window white 100 state renderWorld handleEvent updateWorld

updateWorld :: Float -> State -> State
updateWorld _ state = state

renderWorld :: State -> Picture
renderWorld Playing{game=game, focus=focus} = renderGame game focus


cellLength :: Float
cellLength = 40.0

renderGame :: Game -> Coord -> Picture
renderGame game focus = pictures 
  [
    renderBoard game,
    renderFocus focus
  ]

renderBoard :: Game -> Picture
renderBoard Game{board=board, config=config} = pictures $ concat 
  [
    [
      translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $ 
        renderCell cell (config !! r !! c)
      | (c, cell) <- zip [0..] row
    ] 
    | (r, row) <- zip [0..] board
  ]

renderCell :: Cell -> Int -> Picture
renderCell cell block = pictures 
  [
    color (colorsOfBlocks !! block) $ rectangleSolid cellLength cellLength,
    color white $ rectangleWire cellLength cellLength,
    color black $ translate (- cellLength / 4) (- cellLength / 4) $ 
      scale 0.2 0.2 $ text $ maybe "" show cell
  ]

renderFocus :: Coord -> Picture
renderFocus (r, c) =
  color black $ translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $
    rectangleWire cellLength cellLength


handleEvent :: Event -> State -> State

handleEvent (EventKey (SpecialKey KeyUp) Up _ _) state@(Playing{focus=focus}) = state{focus = moveFocus focus (-1) 0}
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) state@(Playing{focus=focus}) = state{focus = moveFocus focus 1 0}
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) state@(Playing{focus=focus}) = state{focus = moveFocus focus 0 (-1)}
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) state@(Playing{focus=focus}) = state{focus = moveFocus focus 0 1}

handleEvent _ state = state

moveFocus :: Coord -> Int -> Int -> Coord
moveFocus (r, c) dr dc =
  let 
    r' = r + dr
    c' = c + dc
  in
    if 0 <= r' && r' <= 8 && 0 <= c' && c' <= 8 
    then (r', c') else (r, c)