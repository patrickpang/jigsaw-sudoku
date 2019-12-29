{-# LANGUAGE NamedFieldPuns #-}

module State where

import Game
import Logic
import Persistence
import History

import Data.List
import Data.Array
import Data.Char (digitToInt)
import System.Exit (exitSuccess)
import Graphics.Gloss.Interface.IO.Game
  
data State =
  State {game :: Game, history :: History, focus :: Coord, solution :: Board, filename :: Maybe FilePath}

handleEvent :: Event -> State -> IO State

handleEvent (EventKey (SpecialKey KeyUp) Up _ _) state@(State{focus}) = 
  return state{focus = moveFocus focus (-1) 0}
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) state@(State{focus}) = 
  return state{focus = moveFocus focus 1 0}
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) state@(State{focus}) = 
  return state{focus = moveFocus focus 0 (-1)}
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) state@(State{focus}) = 
  return state{focus = moveFocus focus 0 1}

handleEvent (EventKey (SpecialKey k) Up _ _) state@(State{game, focus, history=History{initial}}) 
  | elem k keys = -- Input
    updateAfterMove state $ makeMove game initial focus (fmap (+1) (elemIndex k keys)) 
  | k == KeyDelete || k == KeyBackspace = -- Erase
    updateAfterMove state $ makeMove game initial focus Nothing
  | k == KeyEsc = -- Quit
    exitSuccess
  | otherwise = return state
  where
    keys = [KeyPad1, KeyPad2, KeyPad3, KeyPad4, KeyPad5, KeyPad6, KeyPad7, KeyPad8, KeyPad9]

handleEvent (EventKey (Char c) Up _ _) state@(State{game, focus, solution, history=history@History{initial}})
  | '1' <= c && c <= '9' = -- Input
    updateAfterMove state $ makeMove game initial focus (Just $ digitToInt c)
  | c == '\b' = -- Erase
    updateAfterMove state $ makeMove game initial focus Nothing
  | c == 'h' = -- Hint
    updateAfterMove state $ makeMove game initial focus (solution ! focus)
  | c == 's' = -- Solve
    updateAfterMove state $ game{board = solution}
  | c == 'c' = -- Clear
    updateAfterMove state $ game{board = initial}
  | c == 'u' = -- Undo
    updateAfterUndoRedo state $ undoMove history $ board game
  | c == 'r' = -- Redo
    updateAfterUndoRedo state $ redoMove history $ board game
  | c == 'q' = -- Quit
    exitSuccess
  | otherwise = return state

handleEvent _ state = return state

updateAfterMove :: State -> Game -> IO State
updateAfterMove state@(State{game, history, filename}) game' = do
  if board game /= board game' then do
    let history' = addMove history $ board game
    saveGame game' filename
    saveHistory history' $ locateHistory filename
    return state{game=game', history=history'}
  else do
    return state

updateAfterUndoRedo :: State -> (Board, History) -> IO State
updateAfterUndoRedo state@(State{game, filename}) (board', history') = do
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
