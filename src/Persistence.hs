{-# LANGUAGE NamedFieldPuns #-}

module Persistence where

import Game
import Generator

import Data.List.Split (chunksOf)
import Data.Array
import Data.Char (digitToInt, intToDigit)
import System.Directory (doesFileExist)

instance Show Game where
  show Game{board, blocks} =
    let
      blocksRows = dumpBlocks blocks
      boardRows = dumpBoard board
    in
      unlines $ blocksRows ++ boardRows

saveGame :: Game -> Maybe FilePath -> IO ()
saveGame game (Just filename) = writeFile filename $ show game
saveGame _ Nothing = return ()

loadGame :: Maybe FilePath -> IO Game

loadGame (Just filename) = do
  hasGame <- doesFileExist filename
  if hasGame then do
    content <- readFile filename
    let 
      rows = lines content
      blocksRows = take 9 rows
      boardRows = drop 9 rows

      board = parseBoard boardRows
      blocks = parseBlocks blocksRows
    
    return (Game {board, blocks})
  else loadGame Nothing

loadGame Nothing = generateGame

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
