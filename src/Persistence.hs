module Persistence (loadGame, saveGame) where

import Model

import Data.List.Split (chunksOf)
import Data.Array
import Data.Char (digitToInt, intToDigit)

loadGame :: String -> IO Game
loadGame filename = do
  content <- readFile filename
  let 
    rows = lines content
    blocksRows = take 9 rows
    boardRows = drop 9 rows

    board = parseBoard boardRows
    blocks = parseBlocks blocksRows
  
  return (Game {board=board, blocks=blocks})

saveGame :: Game -> String -> IO ()
saveGame game filename = do
  let
    blocksRows = dumpBlocks $ blocks game
    boardRows = dumpBoard $ board game
    content = unlines (blocksRows ++ boardRows)

  writeFile filename content


parseBoard :: [String] -> Board
parseBoard lines = 
  listArray ((0, 0), (8, 8)) $ 
  concat [[if char == '.' then Nothing else Just (digitToInt char) | char <- line] | line <- lines]

dumpBoard :: Board -> [String]
dumpBoard board = 
  [[maybe '.' intToDigit cell | cell <- row] | row <- chunksOf 9 $ elems board]

parseBlocks :: [String] -> Blocks
parseBlocks lines =
  listArray ((0, 0), (8, 8)) $ 
  concat [[digitToInt char | char <- line] | line <- lines]

dumpBlocks :: Blocks -> [String]
dumpBlocks blocks =
  [[intToDigit block | block <- row] | row <- chunksOf 9 $ elems blocks]