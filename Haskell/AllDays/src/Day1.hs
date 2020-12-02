module Day1 where

import Control.Monad
import Data.List

intList :: [String] -> [Int]
intList = map (\ x -> read x :: Int)

sumIs2020P1 :: [Int] -> (Int,Int)
sumIs2020P1 xs = (a,b) where (a, b):hs =  [(x,y) | x <- xs, y <- xs,  x + y == 2020]

sumIs2020P2 :: [Int] -> Int
sumIs2020P2 xs = a * b * c where (a, b, c):hs =  [(x,y,z) | x <- xs, y <- xs, z <- xs,  x + y + z == 2020]

sumIs2020P1' :: [Int] -> [(Int, Int)]
sumIs2020P1' xs = do 
    x <- xs
    y <- xs
    guard (x + y == 2020)
    pure (x,y)

sumIs2020P1'' :: [Int] -> Int
sumIs2020P1'' xs =  a * b where
    (a,b):rest = filter (\(x,y) -> x + y == 2020)  pairs       

    pairs = xs >>= \x -> 
                    xs >>= \y -> 
                        pure (x,y)
            

   
    

day1Main :: IO () 
day1Main = do
    content <- readFile "data/Day1Part1.txt"
    let (a,b) = sumIs2020P1 . intList . lines $ content 
    print (a*b)
    let p2 = sumIs2020P2 . intList . lines $ content 
    print p2

    let xx = sumIs2020P1'' . intList . lines $ content 
    print  xx
    
   