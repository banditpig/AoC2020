{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
{-# OPTIONS -fwarn-typed-holes #-}

module Day10 where
import Data.Sort

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
fixList vs = (0 : myV : svs, myV) where
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
    