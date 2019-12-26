module Generator where

-- https://hub.darcs.net/thielema/set-cover/browse/example/Random.hs
-- import qualified Random as Random
-- import System.Random (StdGen, getStdGen)

-- randomPuzzles :: MS.State StdGen [[Cell]]
-- randomPuzzles =
--     return . ESC.partitions
--         =<< Random.intSetFromSetAssigns
--         =<< Random.shuffle assigns

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

-- mainGenerate = do
--     gen <- getStdGen
--     case MS.evalState randomPuzzles gen of
--         solution:_ -> do
--             putStrLn $ formatSparse solution
--             let minSolution = minimizePuzzle solution
--             putStrLn $ formatSparse minSolution
--             printf "%d initially filled cells\n\n" $ length minSolution
--         _ -> fail "to few puzzles"