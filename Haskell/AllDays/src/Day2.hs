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
valid1 crit = count >=  lwr crit && count <=  upr crit where
  count = length $ filter (\ch -> ch == letter crit) (pwd crit)


valid2 ::  CritAndPwd -> Bool
valid2 crit = res where
    ch1 = pwd crit !! (lwr crit - 1)
    ch2 = pwd  crit !! (upr crit - 1)
    letr = letter crit
    res = ch1 == letr && ch2 /= letr
        ||
          ch2 == letr && ch1 /= letr

day2Main :: IO () 
day2Main = do
    content <- readFile "data/Day2Part1.txt"
    let critPwds = map mkCritAndPwd (lines  content)
    print "Part 1."
    print $ length . filter  valid1 $ critPwds
    print "Part 2."
    print $ length . filter  valid2 $ critPwds

   