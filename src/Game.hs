{-# LANGUAGE NamedFieldPuns #-}

module Game where

import Data.List.Split (chunksOf)
import Data.Array
import Data.Char (digitToInt, intToDigit)

type Cell = Maybe Int
type Coord = (Int, Int)

type Board = Array Coord Cell
type Blocks = Array Coord Int

data Game = Game {board :: Board, blocks :: Blocks}

instance Show Game where
  show Game{board, blocks} =
    let
      blocksRows = dumpBlocks blocks
      boardRows = dumpBoard board
    in
      unlines $ blocksRows ++ boardRows

saveGame :: Game -> FilePath -> IO ()
saveGame game filename = writeFile filename $ show game

loadGame :: FilePath -> IO Game
loadGame filename = do
  content <- readFile filename
  let 
    rows = lines content
    blocksRows = take 9 rows
    boardRows = drop 9 rows

    board = parseBoard boardRows
    blocks = parseBlocks blocksRows
  
  return (Game {board, blocks})

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