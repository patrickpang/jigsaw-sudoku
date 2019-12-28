module Utils where

import System.Random (randomRIO)
import Data.List
import Data.Function
import Data.Array.IO
import Control.Monad

-- | Sort and group a list based on a function
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f xs = groupBy ((==) `on` f) $ sortOn f xs

-- | Randomly choose an element from a list
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

-- | Randomly shuffle a list
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs