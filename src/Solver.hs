-- Modified from https://hub.darcs.net/thielema/set-cover/browse/example/Sudoku.hs

module Solver (solveGame) where

import Game

-- import qualified Math.SetCover.BitSet as BitSet
-- import qualified Math.SetCover.Bit as Bit
import qualified Math.SetCover.Exact as ESC

import Data.Array
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data X = Pos Int Int | Row Int Int | Column Int Int | Block Int Int
         deriving (Eq, Ord, Show)

type Association = ((Int, Int), Int)
type Assign = ESC.Assign Association

assign :: Int -> Int -> Int -> Int -> Assign (Set X)
assign n r c b =
   ESC.assign ((r, c), n) $
   Set.fromList [Pos r c, Row n r, Column n c, Block n b]

assigns :: Blocks -> [Assign (Set X)]
assigns blocks = [assign n r c b | n <- [1..9], r <- [0..8], c <- [0..8], let b = blocks ! (r, c)]

solveGame :: Game -> Board
solveGame game =
   let
      initAssigns = assigns $ blocks game
      occupied = filter (isJust . snd) $ assocs $ board game
      occupiedAssigns = [assign n r c b | ((r, c), (Just n)) <- occupied, let b = (blocks game) ! (r, c)]
      solution = head $ ESC.search $ foldl (flip ESC.updateState) (ESC.initState initAssigns) occupiedAssigns
   in
      array ((0, 0), (8, 8)) [((r, c), (Just n)) | ((r, c), n) <- solution]