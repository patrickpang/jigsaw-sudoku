{-# LANGUAGE NamedFieldPuns #-}

module History where

import Game

import System.FilePath (replaceExtension)
import System.Directory (doesFileExist)
import Data.List.Split (splitOn)
import Data.Array
import Data.Char (digitToInt, intToDigit)

-- Ref: https://redux.js.org/recipes/implementing-undo-history/
data History = History {initial :: Board, past :: [Board], future :: [Board]} deriving (Show)

undoStep :: History -> Board -> (Board, History)
undoStep history@History{past=(b:bs), future} present = (b, history{past=bs, future=(present:future)})
undoStep history@History{past=[], future} present = (present, history)

redoStep :: History -> Board -> (Board, History)
redoStep history@History{past, future=(b:bs)} present = (b, history{past=(present:past), future=bs})
redoStep history@History{past, future=[]} present = (present, history)

addStep :: History -> Board -> History
addStep history@History{past} present = history{past=(present:past), future=[]}

locateHistory :: FilePath -> FilePath
locateHistory boardFilename = replaceExtension boardFilename ".log"

pastFutureSeparator :: String
pastFutureSeparator = "---"

loadHistory :: FilePath -> Board -> IO History
loadHistory filename initial = do
  hasHistory <- doesFileExist filename
  if hasHistory then do
    content <- readFile filename
    let
      [pastContent, futureContent] = splitOn pastFutureSeparator content
      (initialRow:pastRows) = lines pastContent
      futureRows = lines futureContent
      initial = parseBoardCompact initialRow
      past = map parseBoardCompact pastRows
      future = map parseBoardCompact futureRows
      history = History{initial, past, future}
    return history
  else do
    return History{initial, past=[], future=[]}

saveHistory :: History -> FilePath -> IO ()
saveHistory History{initial, past, future} filename = do
  let
    initialRow = dumpBoardCompact initial
    pastRows = map dumpBoardCompact past
    futureRows = map dumpBoardCompact future
    content = unlines $ [initialRow] ++ pastRows ++ [pastFutureSeparator] ++ futureRows
  
  writeFile filename content

parseBoardCompact :: String -> Board
parseBoardCompact line =
  listArray ((0, 0), (8, 8)) $ 
  [if char == '.' then Nothing else Just (digitToInt char) | char <- line]

dumpBoardCompact :: Board -> String
dumpBoardCompact board = 
  [maybe '.' intToDigit cell | cell <- elems board]
