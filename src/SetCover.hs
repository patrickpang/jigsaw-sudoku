module SetCover where

import Game

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Math.SetCover.Exact as ESC

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