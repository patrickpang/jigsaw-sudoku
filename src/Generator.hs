{-# LANGUAGE NamedFieldPuns #-}

-- Modified from https://hub.darcs.net/thielema/set-cover/browse/example/Sudoku.hs
module Generator where

import Game
import Utils (pick, shuffle)
import Solver (assigns)

import qualified Math.SetCover.Exact as ESC

import Data.List
import Data.Array
import Data.Graph

generateGame :: IO Game
generateGame = do
  blocks <- generateBlocks
  board <- generateBoard blocks
  return Game{board, blocks}

-- generate blocks

generateBlocks :: IO Blocks
generateBlocks = do
  -- start with regular blocks layout
  reshape regularBlocks 
  where
    reshape :: Blocks -> IO Blocks
    reshape blocks = do
      let blockPeers = map (blockCells blocks) [0..8] 
      -- repeat until no regular blocks
      if not $ any isRegularBlock blockPeers 
      then do
        return blocks
      else do
        --  pick an outer cell from a random regular block
        cells <- pick $ filter isRegularBlock blockPeers
        let outer = outerCellsOfRegularBlock cells
        cell <- pick outer
        let neighbors = validNeighbors blocks cell

        -- retry if no valid neighbors
        if null neighbors 
        then reshape blocks
        else do
          neighbor <- pick neighbors
          --  swap an outer cell with a neighbor cell of another block
          let blocks' = swapCells blocks cell neighbor

          --  retry if the blocks become disconnected
          if all isConnectedBlock $ map (blockCells blocks') [blocks ! cell, blocks ! neighbor]
          then reshape blocks'
          else reshape blocks

regularBlocks :: Blocks
regularBlocks = 
  array ((0, 0), (8, 8)) $ 
  [((r, c), (r `div` 3) * 3 + (c `div` 3)) | r <- [0..8], c <- [0..8]]

swapCells :: Blocks -> Coord -> Coord -> Blocks
swapCells blocks c1 c2 =
  let
    b1 = blocks ! c1
    b2 = blocks ! c2
  in
    blocks // [(c1, b2), (c2, b1)]

validNeighbors :: Blocks -> Coord -> [Coord]
validNeighbors blocks (r, c) =
  filter (\cell -> blocks ! cell /= blocks ! (r, c)) $
  filter (inRange $ bounds blocks) $
  [(r-1, c-1), (r-1, c+1), (r+1, c-1), (r+1, c+1)]

blockCells :: Blocks -> Int -> [Coord]
blockCells blocks i =
  [coord | (coord, block) <- assocs blocks, block == i]

outerCellsOfRegularBlock :: [Coord] -> [Coord]
outerCellsOfRegularBlock cells =
  filter (/= (r, c)) cells
  where
    r = 1 + (minimum $ map fst cells)
    c = 1 + (minimum $ map snd cells)

isConnectedBlock :: [Coord] -> Bool
isConnectedBlock cells =
  (length $ components graph) == 1
  where
    neighbors :: [Coord] -> Coord -> [Coord] 
    neighbors peers (r, c) = 
      filter (\cell -> elem cell peers) [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
    
    graph :: Graph
    (graph, _, _) = graphFromEdges [(cell, cell, neighbors cells cell) | cell <- cells] 
    
isRegularBlock :: [Coord] -> Bool
isRegularBlock cells =
  length rows == 3 && length columns == 3
  where
    rows = nub $ map fst cells
    columns = nub $ map snd cells

-- generate board

generateBoard :: Blocks -> IO Board
generateBoard blocks = do
  a <- shuffle $ assigns blocks
  let
    solution = head $ ESC.partitions a
    board = array ((0, 0), (8, 8)) [((r, c), (Just n)) | ((r, c), n) <- solution]
  return $ minimizeBoard board

-- minimize: a single solution

minimizeBoard :: Board -> Board
minimizeBoard board = board

-- minimizePuzzle :: [Cell] -> [Cell]
-- minimizePuzzle =
--     let asnMap = foldMap (\asn -> Map.singleton (ESC.label asn) asn) bitAssigns
--         lookupAssign =
--             flip (Map.findWithDefault (error "coordinates not available")) asnMap
--         go state xs (y:ys) =
--             case ESC.search $ foldl (flip ESC.updateState) state ys of
--             [_] -> go state xs ys
--             _ -> go (ESC.updateState y state) (ESC.label y : xs) ys
--         go _ xs [] = xs
--     in  go (ESC.initState bitAssigns) [] . map lookupAssign
