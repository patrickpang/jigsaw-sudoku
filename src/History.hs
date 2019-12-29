{-# LANGUAGE NamedFieldPuns #-}

module History where

import Game

import System.FilePath (replaceExtension)
import System.Directory (doesFileExist)
import Data.List.Split (splitWhen)
import Data.Array
import Data.Char (digitToInt, intToDigit)

-- Ref: https://redux.js.org/recipes/implementing-undo-history/
data History = History {initial :: Board, past :: [Board], future :: [Board]} deriving (Show)

undoMove :: History -> Board -> (Board, History)
undoMove history@History{past=(b:bs), future} present = (b, history{past=bs, future=(present:future)})
undoMove history@History{past=[]} present = (present, history)

redoMove :: History -> Board -> (Board, History)
redoMove history@History{past, future=(b:bs)} present = (b, history{past=(present:past), future=bs})
redoMove history@History{future=[]} present = (present, history)

addMove :: History -> Board -> History
addMove history@History{past} present = history{past=(present:past), future=[]}

countMoves :: History -> Int
countMoves history@History{past} = length past

locateHistory :: Maybe FilePath -> Maybe FilePath
locateHistory = fmap (\boardFilename -> replaceExtension boardFilename ".log")

pastFutureSeparator :: String
pastFutureSeparator = "---"

loadHistory :: Maybe FilePath -> Board -> IO History

loadHistory (Just filename) present = do
  hasHistory <- doesFileExist filename
  if hasHistory then do
    content <- readFile filename
    let
      [(initialRow:pastRows), futureRows] = splitWhen (== pastFutureSeparator) $ lines content
      initial = parseBoardCompact initialRow
      past = map parseBoardCompact pastRows
      future = map parseBoardCompact futureRows
      history = History{initial, past, future}
    return history
  else loadHistory Nothing present

loadHistory Nothing present = return History{initial=present, past=[], future=[]}

saveHistory :: History -> Maybe FilePath -> IO ()

saveHistory History{initial, past, future} (Just filename) = do
  let
    initialRow = dumpBoardCompact initial
    pastRows = map dumpBoardCompact past
    futureRows = map dumpBoardCompact future
    content = unlines $ [initialRow] ++ pastRows ++ [pastFutureSeparator] ++ futureRows
    
  writeFile filename content
    
saveHistory _ Nothing = return ()
    
parseBoardCompact :: String -> Board
parseBoardCompact line =
  listArray ((0, 0), (8, 8)) $ 
  [if char == '.' then Nothing else Just (digitToInt char) | char <- line]

dumpBoardCompact :: Board -> String
dumpBoardCompact board = 
  [maybe '.' intToDigit cell | cell <- elems board]
