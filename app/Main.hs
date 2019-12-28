{-# LANGUAGE NamedFieldPuns #-}

module Main where
  
import Game
import Logic
import Persistence
import History
import Solver (solveGame)
import Help

import Colors

import Data.List
import Data.List.Split (splitOn)
import Data.Array
import Data.Char (digitToInt)
import Data.Maybe
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> mainHelp
    ["--help"] -> mainHelp
    [filename] -> mainGame $ Just filename
    [] -> mainGame Nothing
    _ -> mainHelp

mainHelp :: IO ()
mainHelp = do
  putStrLn "jigsaw-sudoku"
  -- TODO: docopt format USAGE

data State =
  Playing {game :: Game, history :: History, focus :: Coord, solution :: Board, filename :: Maybe FilePath}

mainGame :: Maybe FilePath -> IO ()
mainGame filename = do
  game <- loadGame filename
  history <- loadHistory (locateHistory filename) (board game)
  let 
    solution = solveGame game
    state = Playing{game, focus=(0,0), solution, filename, history}
  
  playIO FullScreen white 100 state renderWorld handleEvent updateWorld

updateWorld :: Float -> State -> IO State
updateWorld _ state = return state

renderWorld :: State -> IO Picture
renderWorld Playing{game, focus, filename} = return $ pictures 
  [
    renderGame game focus,
    renderHeader filename,
    renderUsage,
    renderCommands
  ]

renderHeader :: Maybe FilePath -> Picture
renderHeader filename = 
  pictures [brand, author, title, autosave]
  where
    brand = translate (-140) 300 $ scale 0.3 0.3 $ text "Jigsaw Sudoku"
    author = translate (-80) 270 $ scale 0.1 0.1 $ text "made by Patrick Pang"

    title = translate (-180) 200 $ scale 0.2 0.2 $ text $ maybe "" takeBaseName filename
    autosave = translate 120 200 $ scale 0.1 0.1 $ text $ if isJust filename then "autosaved" else ""
  
renderUsage :: Picture
renderUsage = translate (-600) 200 $ renderText usageString

renderCommands :: Picture
renderCommands = translate 260 200 $ renderText commandsString

renderText :: String -> Picture
renderText s = pictures
  [
    translate 0 (i * (-height)) $ scale 0.1 0.1 $ text line
    | (i, line) <- zip [0..] $ splitOn "\n" s
  ]
  where height = 20

cellLength :: Float
cellLength = 40.0

renderGame :: Game -> Coord -> Picture
renderGame game focus = pictures 
  [
    renderBoard game conflicts,
    renderFocus focus,
    renderStatus conflicts
  ]
  where
    conflicts = allConflicts game

renderBoard :: Game -> [Coord] -> Picture
renderBoard game conflicts = pictures
  [
    translate (((fromIntegral c) - 4) * cellLength) ((4 - (fromIntegral r)) * cellLength) $ 
      renderCell cell block hasConflicts
    | ((r, c), cell) <- assocs $ board game,
    let block = (blocks game) ! (r,c), 
    let hasConflicts = elem (r, c) conflicts
  ]

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

renderStatus :: [Coord] -> Picture
renderStatus conflicts =
  translate (-180) (-200) $ scale 0.1 0.1 $ text $ "No. of conflicts: " ++ show (length conflicts)

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
  | k == KeyEsc = -- Quit
    exitSuccess
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
  | c == 'q' = -- Quit
    exitSuccess
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
