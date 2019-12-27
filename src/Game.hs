{-# LANGUAGE NamedFieldPuns #-}

module Game where

import Data.List.Split (chunksOf)
import Data.Array
import Data.Char (digitToInt, intToDigit)

type Cell = Maybe Int
type Coord = (Int, Int)

type Board = Array Coord Cell
type Blocks = Array Coord Int

data Game = Game {board :: Board, blocks :: Blocks} deriving (Show)

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

saveGame :: Game -> FilePath -> IO ()
saveGame Game{board, blocks} filename = do
  let
    blocksRows = dumpBlocks blocks
    boardRows = dumpBoard board
    content = unlines $ blocksRows ++ boardRows

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