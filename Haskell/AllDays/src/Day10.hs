{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS -Wall -fno-warn-type-defaults #-}
{-# OPTIONS -fwarn-typed-holes #-}
-- https://stackoverflow.com/questions/11168238/haskell-generating-all-paths-between-nodes
module Day10 where
import  Data.Sort 
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as M


   

buildGraph :: [Int] -> IntMap [Int]
buildGraph xs = 
    foldl (\acc x  -> IntMap.insert x (nodeList x xs) acc ) IntMap.empty xs
    where 
        nodeList x xs = [ x + j | j <- [1,2,3], (x + j) `elem` xs]


connect :: Int -> Int -> IntMap [Int] -> [[(Int, Int)]]
connect x y g = helper x y g [x]
  where
    helper a b g visited
        | a == b    = [[]]
        | otherwise = [(a,c):path | c <- neigh a, c `notElem` visited, path <- helper c b g (c:visited)]
        where
            neigh i = IntMap.findWithDefault [] i g




diffs :: [Int] -> [Int]
diffs xs@(_:xxs) = zipWith (-) xxs xs

freqs :: Ord a => [a] -> M.Map a Int
freqs = M.fromListWith (+) . map (,1) 

next :: Int -> [Int] -> Int
next v vs
    | (v + 1) `elem` vs = v + 1
    | otherwise = next (v + 1) vs

buildList :: Int ->  Int -> [Int] -> [Int]
buildList done  v vs
    | v == done  = [v]
    | otherwise = v : buildList done  nxtv vs
    where nxtv = next v vs
 
fixList :: [Int] -> ([Int], Int)
fixList vs = (0  : svs ++ [myV], myV) where
    svs = sort vs
    myV = 3 + last svs

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (a, b) (c, d) = (a + c, b + d)

-- (3s, 1s)
threesOnes :: [Int] -> (Int, Int)
threesOnes [] = (0, 0)
threesOnes [a, b] = if  b - a == 3 then  (1, 0) else (0 , 1)
threesOnes (a:b:xs)
    |  b - a == 3  = (1, 0) `addPairs` threesOnes (b:xs)
    |  otherwise   = (0, 1) `addPairs` threesOnes (b:xs)
   
day10Main:: IO ()
day10Main = do
    inp <- readFile "../data/Day10Part1.txt" 
    let nums = map (\s -> read s::Int) $ lines inp
    let  (vs, done) = fixList nums
    let (a,b) = threesOnes $ buildList  done 0 vs 
    print ( a * b )
   
    print "part2"
    -- OK print (paths  (IntSet.fromList  vs))
    let g = buildGraph vs
    print g
    let con = connect 0 22 g
    print con
    print (length con)  