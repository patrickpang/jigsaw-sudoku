{-# LANGUAGE NamedFieldPuns #-}

module Main where
  
import Game
import Logic
import History
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
  Playing {game :: Game, history :: History, focus :: Coord, solution :: Board, filename :: FilePath}

mainGame :: String -> IO ()
mainGame filename = do
  game <- loadGame filename
  history <- loadHistory (locateHistory filename) (board game)
  -- TODO: generate for new file
  (screenWidth, screenHeight) <- getScreenSize
  let 
    solution = solveGame game
    state = Playing{game, focus=(0,0), solution, filename, history}

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

handleEvent (EventKey (SpecialKey k) Up _ _) state@(Playing{game, focus, history=History{initial}}) 
  | elem k keys = -- Input
    updateAfterMove state $ makeMove game initial focus (fmap (+1) (elemIndex k keys)) 
  | k == KeyDelete || k == KeyBackspace = -- Erase
    updateAfterMove state $ makeMove game initial focus Nothing
  | otherwise = return state
  where
    keys = [KeyPad1, KeyPad2, KeyPad3, KeyPad4, KeyPad5, KeyPad6, KeyPad7, KeyPad8, KeyPad9]

handleEvent (EventKey (Char c) Up _ _) state@(Playing{game, focus, solution, history=history@History{initial}})
  | '1' <= c && c <= '9' = -- Input
    updateAfterMove state $ makeMove game initial focus (Just $ digitToInt c)
  | c == '\b' = -- Erase
    updateAfterMove state $ makeMove game initial focus Nothing
  | c == 'h' = -- Hint
    updateAfterMove state $ makeMove game initial focus (solution ! focus)
  | c == 's' = -- Solve
    updateAfterMove state $ game{board = solution}
  | c == 'u' = -- Undo
    updateAfterUndoRedo state $ undoMove history $ board game
  | c == 'r' = -- Redo
    updateAfterUndoRedo state $ redoMove history $ board game
  | otherwise = return state

handleEvent _ state = return state

updateAfterMove :: State -> Game -> IO State
updateAfterMove state@(Playing{game, history, filename}) game' = do
  if board game /= board game' then do
    let history' = addMove history $ board game
    saveGame game' filename
    saveHistory history' $ locateHistory filename
    return state{game=game', history=history'}
  else do
    return state

updateAfterUndoRedo :: State -> (Board, History) -> IO State
updateAfterUndoRedo state@(Playing{game, filename}) (board', history') = do
  if board' /= board game then do
    let game' = game{board=board'}
    saveGame game' filename
    saveHistory history' $ locateHistory filename
    return state{game=game', history=history'}
  else do
    return state

moveFocus :: Coord -> Int -> Int -> Coord
moveFocus (r, c) dr dc =
  let 
    r' = r + dr
    c' = c + dc
  in
    if 0 <= r' && r' <= 8 && 0 <= c' && c' <= 8 
    then (r', c') else (r, c)
