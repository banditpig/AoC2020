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
_MAXx =  96 --9
_MAXy =  92 --9

type Point = (Int, Int) 
data Cell = Empty | Occupied | Floor deriving (Eq, Show, Generic)
instance Hashable Cell

type Grid = Map Point Cell

neighbours :: Point -> [Point]
neighbours (x,y)
      = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0) && inBounds (x + m, y + n) ]

-- radiate out use func to see if an occupioed
-- seat is visible 
spotOccupiedSeat :: Grid -> Point -> (Point -> Point) -> Bool
spotOccupiedSeat g p f

    | not (inBounds p')     = False
    | g Map.! p' == Occupied = True
    | g Map.! p' == Empty    = False
    | otherwise             = spotOccupiedSeat g p' f 
    where 
        p' = f p
-- step fucntions to radiate out
fx0  (x,y) = (x+1,y)
fx0' (x,y) = (x-1,y)
f0y  (x,y) = (x, y+1)
f0y' (x,y) = (x, y-1)

tl (x,y) = (x-1,y-1)
tr (x,y) = (x+1,y-1)
bl (x,y) = (x-1,y+1)
br (x,y) = (x+1,y+1)
funcs = [br,tl,bl,tr, fx0,fx0' ,f0y ,f0y' ]

canFindOccupiedSeats :: Grid -> Int -> Point -> (Int, Int)
canFindOccupiedSeats g target p = (occ, empty)  where
    bools =  [spotOccupiedSeat g p br,
            spotOccupiedSeat g p tl,
            spotOccupiedSeat g p bl,
            spotOccupiedSeat g p tr,

            spotOccupiedSeat g p fx0,
            spotOccupiedSeat g p fx0',
            spotOccupiedSeat g p f0y,
            spotOccupiedSeat g p f0y']
    (occ, empty) = foldr(\b (o, e) -> if b then (o+1,e) else (o, e+1)) (0,0) bools

    
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
            go _ _ _                                    = Nothing 
            count x = length . filter (== x)

step' :: Grid -> Grid
step' g = mergeListWithMap changes g  where
        changes =  mapMaybe (\ p -> nextStep (p, g Map.! p)) (Map.keys g)
        
        nextStep :: (Point, Cell)  -> Maybe (Point, Cell)
        nextStep (p, cell) = go p cell  where
            (occ, emp) = canFindOccupiedSeats g 5 p
            go::Point ->  Cell  -> Maybe (Point, Cell)
            go p Empty = if occ == 0 then  Just (p, Occupied) else Just (p, Empty)
            go p Occupied =  if occ>=5 then Just (p, Empty ) else Just (p, Occupied)
            go _ _  = Nothing 
               
            
parseInitiaGrid :: B.ByteString -> (Grid, Int, Int)
parseInitiaGrid s =  (go, maxX, maxY) 
  where
    ls = B.split '\n' s
    maxY = length ls
    maxX = B.length (head ls)
    points = [(x, y) | x <- [0 .. 96], y <- [0 .. 92] ] -- 92, 96
    go  = foldr insert Map.empty  points where
      insert :: Point -> Grid -> Grid
      insert (x,y) g 
        | B.index (ls !! y) x == 'L' = Map.insert (x, y) Empty g
        | B.index (ls !! y) x == '.' = Map.insert (x, y) Floor g
        | B.index (ls !! y) x == '#' = Map.insert (x, y) Occupied g
        | otherwise                  = Map.insert (x, y) Floor  g -- won't happen

part1 :: Grid -> (Int,Grid)
part1  = go 0 []  where
    go::Int -> [Int]  -> Grid -> (Int,Grid)
    go cnt hashes  g
        | hsh `elem` hashes = (cnt, g)
        | otherwise = go (cnt+1) (hsh:hashes)  (step' g) where
            hsh = hash (Map.toList g)

day11Main = do
    inp <- readFile "../data/Day11Part1.txt"
    (initialGrid, x, y) <- parseInitiaGrid <$> B.readFile "../data/Day11Part1.txt"
    print x
    print y
    let (itr, grid) = part1 initialGrid
    let occ = length $ filter (\(_, c) -> c == Occupied ) (Map.toList grid)
    print occ