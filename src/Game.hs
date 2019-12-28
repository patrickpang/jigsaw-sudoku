{-# LANGUAGE NamedFieldPuns #-}

module Game where

import Data.Array

type Cell = Maybe Int
type Coord = (Int, Int)

type Board = Array Coord Cell
type Blocks = Array Coord Int

data Game = Game {board :: Board, blocks :: Blocks}
