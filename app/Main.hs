{-# LANGUAGE NamedFieldPuns #-}

module Main where
  
import Persistence
import Model
import Logic
import Solver (solveGame)

import Colors

import Data.List
import Data.Array
import Data.Char (digitToInt)
import System.Environment (getArgs)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> mainHelp
    ["--help"] -> mainHelp
    [filename] -> mainGame filename
    [] -> mainGame "maps/map-test.txt" -- TODO: generate
    _ -> mainHelp

mainHelp :: IO ()
mainHelp = do
  putStrLn "jigsaw-sudoku"
  -- TODO: docopt format USAGE

data State =
  Playing {game :: Game, focus :: Coord, solution :: Board, filename :: String}

mainGame :: String -> IO ()
mainGame filename = do
  game <- loadGame filename
  -- TODO: generate for new file
  (screenWidth, screenHeight) <- getScreenSize
  let 
    solution = solveGame game
    state = Playing{game, focus=(0,0), solution, filename}

    windowWidth = 360
    windowHeight = 360
    windowLeft = (screenWidth - windowWidth) `div` 2
    windowTop = (screenHeight - windowHeight) `div` 2
    window = InWindow "Jigsaw Sudoku" (windowWidth, windowHeight) (windowLeft, windowTop)
  
  playIO window white 100 state renderWorld handleEvent updateWorld
  
updateWorld :: Float -> State -> IO State
updateWorld _ state = return state

renderWorld :: State -> IO Picture
renderWorld Playing{game, focus} = renderGame game focus


cellLength :: Float
cellLength = 40.0

renderGame :: Game -> Coord -> IO Picture
renderGame game focus = return $ pictures 
  [
    renderBoard game,
    renderFocus focus
  ]

renderBoard :: Game -> Picture
renderBoard game = pictures
  [
    translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $ 
      renderCell cell ((blocks game) ! (r,c)) (elem (r, c) conflicts)
    | ((r, c), cell) <- assocs $ board game
  ]
  where
    conflicts = allConflicts game

renderCell :: Cell -> Int -> Bool -> Picture
renderCell cell block hasConflicts = pictures 
  [
    color (colorsOfBlocks !! block) $ rectangleSolid cellLength cellLength,
    color white $ rectangleWire cellLength cellLength,
    color (if hasConflicts then red else black) $ translate (- cellLength / 4) (- cellLength / 4) $ 
      scale 0.2 0.2 $ text $ maybe "" show cell
  ]

renderFocus :: Coord -> Picture
renderFocus (r, c) =
  color black $ translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $
    rectangleWire cellLength cellLength


handleEvent :: Event -> State -> IO State

handleEvent (EventKey (SpecialKey KeyUp) Up _ _) state@(Playing{focus}) = 
  return state{focus = moveFocus focus (-1) 0}
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) state@(Playing{focus}) = 
  return state{focus = moveFocus focus 1 0}
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) state@(Playing{focus}) = 
  return state{focus = moveFocus focus 0 (-1)}
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) state@(Playing{focus}) = 
  return state{focus = moveFocus focus 0 1}

handleEvent (EventKey (SpecialKey k) Up _ _) state@(Playing{game, focus, filename}) 
  | elem k keys = do -- Input
    let game' = makeMove game focus (fmap (+1) (elemIndex k keys))
    saveGame game' filename
    return state{game=game'} 
  | k == KeyDelete || k == KeyBackspace = do -- Erase
    let game' = makeMove game focus Nothing
    saveGame game' filename
    return state{game=game'}
  | otherwise = return state
  where
    keys = [KeyPad1, KeyPad2, KeyPad3, KeyPad4, KeyPad5, KeyPad6, KeyPad7, KeyPad8, KeyPad9]

handleEvent (EventKey (Char c) Up _ _) state@(Playing{game, focus, solution, filename})
  | '1' <= c && c <= '9' = do -- Input
    let game' = makeMove game focus (Just $ digitToInt c)
    saveGame game' filename
    return state{game=game'}
  | c == '\b' = do -- Erase
    let game' = makeMove game focus Nothing
    saveGame game' filename
    return state{game=game'}
  | c == 'h' = do -- Hint
    let game' = makeMove game focus (solution ! focus)
    saveGame game' filename
    return state{game=game'}
  | c == 's' = do -- Solve
    let game' = game{board = solution}
    saveGame game' filename
    return state{game=game'}
  | otherwise = return state

handleEvent _ state = return state

moveFocus :: Coord -> Int -> Int -> Coord
moveFocus (r, c) dr dc =
  let 
    r' = r + dr
    c' = c + dc
  in
    if 0 <= r' && r' <= 8 && 0 <= c' && c' <= 8 
    then (r', c') else (r, c)
