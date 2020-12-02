{-# LANGUAGE RecordWildCards #-}
module Day2 where

import Data.List
import Data.List.Split
import Data.String.Utils

data CritAndPwd = CritAndPwd { 
  lwr :: Int, 
  upr :: Int,
  letter :: Char,
  pwd :: String 
} deriving (Show) 

mkCritAndPwd :: String -> CritAndPwd
mkCritAndPwd s = crtPwd where 
    
    [crit, pwd] = splitOn ":" s
    [rng, letter] =  splitOn " "  crit 
    [lwr, upr] =  splitOn "-"  rng 
    crtPwd = CritAndPwd (read lwr::Int) (read upr::Int) (head letter) (strip pwd)

 
valid1 ::  CritAndPwd -> Bool
valid1 CritAndPwd{..} = count >= lwr && count <= upr where
  count = length . filter (== letter) $ pwd


valid2 ::  CritAndPwd -> Bool
valid2 CritAndPwd{..}  = res where
    ch1 = pwd !! (lwr - 1)
    ch2 = pwd !! (upr  - 1)
    res = ch1 == letter && ch2 /= letter
        ||
          ch2 == letter && ch1 /= letter

day2Main :: IO () 
day2Main = do
    content <- readFile "data/Day2Part1.txt"
    let critPwds = map mkCritAndPwd (lines  content)
    print "Part 1."
    print $ length . filter  valid1 $ critPwds
    print "Part 2."
    print $ length . filter  valid2 $ critPwds

   