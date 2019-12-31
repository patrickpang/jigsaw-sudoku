{-# LANGUAGE NamedFieldPuns #-}

module Game where

import Data.Array

type Cell = Maybe Int
type Coord = (Int, Int)

type Board = Array Coord Cell -- numbers on board
type Blocks = Array Coord Int -- blocks layout

data Game = Game {board :: Board, blocks :: Blocks}
