module Model where

type Cell = Maybe Int
type Coord = (Int, Int)
type Board = [[Cell]]

type Config = [[Int]]
type Blocks = [[Coord]]

data Game = Game { board :: Board, config :: Config, blocks :: Blocks } deriving (Show)