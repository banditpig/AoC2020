module Day3 where

import Control.Monad
import Data.List

type Point = (Int, Int)
type TreeMap = [String]
tree = '#'
f1 (x, y) = (x + 1, y + 1)
f2 (x, y) = (x + 3, y + 1)
f3 (x, y) = (x + 5, y + 1)
f4 (x, y) = (x + 7, y + 1)
f5 (x, y) = (x + 1, y + 2)
funcs = [f1, f2, f3, f4, f5]

itemAt :: Point -> TreeMap -> Char
itemAt (x, y) tm  
    | y >= length tm = '.' 
    | otherwise = (tm !! y) !! (x `mod` 31)

allPoints :: Int -> (Point -> Point) -> [Point]  
allPoints n f = foldr(\x acc -> f (head acc) : acc  ) [(0,0)] [1..n]


part1 :: TreeMap -> (Point -> Point) -> Int
part1 tm f = res where
    size = length tm - 1
    res = length . filter (== tree) . map (`itemAt` tm) $ allPoints size f

part2 :: TreeMap -> Int
part2 tm = product . map (part1 tm) $ funcs 
 
day3Main :: IO () 
day3Main = do
    content <- readFile "data/Day3Part1.txt"
    print $ part1 (lines content) f2
    print $ part2 (lines content)
   