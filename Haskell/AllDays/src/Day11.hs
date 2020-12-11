{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fwarn-typed-holes #-}
module Day11 where 
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map, keys)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.Hashable
import GHC.Generics
import Debug.Trace

import           Data.Set        (Set)
import qualified Data.Set        as S
_MAXx = 92
_MAXy = 96

type Point = (Int, Int) 
data Cell = Empty | Occupied | Floor deriving (Eq, Show, Generic)
instance Hashable Cell

type Grid = Map Point Cell

neighbours :: Point -> [Point]
neighbours (x,y)
      = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0) && inBounds (x + m, y + n) ]

inBounds :: Point -> Bool
inBounds (x, y) = (x >= 0) && (x <= _MAXx) 
                  &&
                  (y >= 0) && (y <= _MAXy)

mergeListWithMap :: [(Point, Cell)] -> Grid -> Grid
mergeListWithMap lst g = g' where
    g' = Map.unions [Map.fromList lst, g]

step :: Grid -> Grid
step g = mergeListWithMap changes g  where
        changes =  mapMaybe (\ p -> nextStep (p, g Map.! p)) (Map.keys g)
        
        nextStep :: (Point, Cell)  -> Maybe (Point, Cell)
        nextStep (p, cell) = go p cell ( map (g Map.!) (neighbours p)) where

            go::Point ->  Cell -> [Cell] -> Maybe (Point, Cell)
            go p Empty adj    | count Occupied adj == 0 = Just (p, Occupied)
            go p Occupied adj | count Occupied adj >= 4 = Just (p, Empty )
            go _ _ _ = Nothing 

            count x = length . filter (== x)

parseInitiaGrid :: B.ByteString -> (Grid, Int, Int)
parseInitiaGrid s =  (go, maxX, maxY) 
  where
    ls = B.split '\n' s
    maxY = length ls
    maxX = B.length (head ls)
    points = [(x, y) | x <- [0 .. 92], y <- [0 .. 96] ]
    go  = foldr insert Map.empty  points where
      insert :: Point -> Grid -> Grid
      insert (x,y) g 
        | B.index (ls !! x) y == 'L' = Map.insert (x, y) Empty g
        | B.index (ls !! x) y == '.' = Map.insert (x, y) Floor g
        | B.index (ls !! x) y == '#' = Map.insert (x, y) Occupied g
        | otherwise                  = Map.insert (x, y) Floor  g -- won't happen

part1 :: Grid -> (Int,Grid)
part1  = go 0 []  where
    go::Int -> [Int]  -> Grid -> (Int,Grid)
    go cnt hashes  g
        | hsh `elem` hashes = (cnt, g)
        | otherwise = go (cnt+1) (hsh:hashes)  (step g) where
            hsh = hash (Map.toList g)

day11Main = do
    inp <- readFile "../data/Day11Part1.txt"
    (initialGrid, x, y) <- parseInitiaGrid <$> B.readFile "../data/Day11Part1.txt"
    
    let (itr, grid) = part1 initialGrid
    let occ = length $ filter (\(_, c) -> c == Occupied ) (Map.toList grid)
    print occ