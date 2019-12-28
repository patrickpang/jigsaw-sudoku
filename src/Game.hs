{-# LANGUAGE NamedFieldPuns #-}

module Game where

import Data.List.Split (chunksOf)
import Data.Array
import Data.Char (digitToInt, intToDigit)
import Data.Maybe
import Control.Monad (when)

type Cell = Maybe Int
type Coord = (Int, Int)

type Board = Array Coord Cell
type Blocks = Array Coord Int

data Game = Game {board :: Board, blocks :: Blocks}
