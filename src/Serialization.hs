module Serialization (loadGame, saveGame) where

import Model

import Data.List
import Data.Char (digitToInt, intToDigit)
import qualified Data.Map as Map

parseBoard :: [String] -> Board
parseBoard lines = 
  [[if char == '.' then Nothing else Just (digitToInt char) | char <- line] | line <- lines]

dumpBoard :: Board -> [String]
dumpBoard board = 
  [[maybe '.' intToDigit cell | cell <- row] | row <- board]

parseConfig :: [String] -> Config
parseConfig lines =
  [[digitToInt char | char <- line] | line <- lines]

dumpConfig :: Config -> [String]
dumpConfig config =
  [[intToDigit block | block <- row] | row <- config]

genBlocks :: Config -> Blocks
genBlocks config =
  Map.elems $ Map.fromListWith (++) $ concat [[(block, [(r, c)]) | (c, block) <- zip [0..] row] | (r, row) <- zip [0..] config]

loadGame :: String -> IO Game
loadGame filename = do
  content <- readFile filename
  let 
    rows = lines content
    configRows = take 9 rows
    boardRows = drop 9 rows

    board = parseBoard boardRows
    config = parseConfig configRows
    blocks = genBlocks config
  
  return (Game {board=board, config=config, blocks=blocks})

saveGame :: Game -> String -> IO ()
saveGame game filename = do
  let
    configRows = dumpConfig $ config game
    boardRows = dumpBoard $ board game
    content = unlines (configRows ++ boardRows)

  writeFile filename content
