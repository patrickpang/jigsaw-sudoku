{-# LANGUAGE NamedFieldPuns #-}

module Main where
  
import Game
import Logic
import Persistence
import History
import Solver (solveGame)
import State
import GamePicture
import HelpPicture

import System.Environment (getArgs)
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
mainHelp = putStrLn usageString

mainGame :: Maybe FilePath -> IO ()
mainGame filename = do
  game <- loadGame filename
  history <- loadHistory (locateHistory filename) (board game)
  let 
    solution = solveGame $ game{board = initial history}
    state = State{game, focus=(0,0), solution, filename, history, showHelp=True}
  
  playIO FullScreen white 100 state renderWorld handleEvent updateWorld

updateWorld :: Float -> State -> IO State
updateWorld _ state = return state

renderWorld :: State -> IO Picture
renderWorld State{game, history, focus, filename, showHelp} = return $ pictures $
  [
    renderHeader filename,
    renderBoard game (initial history) conflicts,
    renderFocus focus,
    renderStatus conflicts ended steps
  ] ++ if showHelp then [renderUsage, renderCommands, renderRules] else []
  where
    conflicts = allConflicts game
    ended = isEnded game
    steps = countMoves history