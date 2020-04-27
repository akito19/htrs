module HTRS.SCC (sccs) where

import Data.List
import Data.Graph

sccsInt :: (Int, Int) -> [(Int, Int)] -> [[Int]]
sccsInt (m, n) es =
    case topSort g of
      []     -> []
      x : xs -> sccsInt' g [x] xs
    where
        g = buildG (m, n) es

sccsInt' g xs@(x : _) []       = [xs]
sccsInt' g xs@(x : _) (y : ys)
  | path g y x = sccsInt' g (y : xs) ys
  | otherwise  = xs : sccsInt' g [y] ys

index :: Eq a => [a] -> a -> Int
index xs x
  | Just i <- elemIndex x xs = i

indexEdge :: Eq a => [a] -> (a, a) -> (Int, Int)
indexEdge xs (x, y) = (index xs x, index xs y)

indexEdges :: Eq a => [a] -> [(a, a)] -> [(Int, Int)]
indexEdges vs es = [ indexEdge vs e | e <- es ]

sccs :: Eq a => [a] -> [(a, a)] -> [[a]]
sccs vs es = [ [ vs !! x | x <- xs ] | xs <- xss ]
    where
        xss = sccsInt (0, length vs - 1) (indexEdges vs es)
