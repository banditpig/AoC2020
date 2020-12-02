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
    [lwr, upr] = map (read::String->Int) . splitOn "-" $  rng 
    crtPwd = CritAndPwd lwr upr (head letter) (strip pwd)

 
valid1 ::  CritAndPwd -> Bool
valid1 CritAndPwd{..} = count >= lwr && count <= upr where
  count = length . filter (== letter) $ pwd


valid2 ::  CritAndPwd -> Bool
valid2 CritAndPwd{..} = count == 1  where
    count = length . filter (== letter) $ [pwd !! (lwr - 1), pwd !! (upr  - 1)]

   

day2Main :: IO () 
day2Main = do
    content <- readFile "data/Day2Part1.txt"
    let critPwds =  mkCritAndPwd <$> lines  content
    print "Part 1."
    print $ length . filter  valid1 $ critPwds
    print "Part 2."
    print $ length . filter  valid2 $ critPwds

   