module Day6 where 
import Data.List.Split ( splitOn )
import Data.List ( nub )
import Data.Set hiding (foldr, map)

groups :: String  -> [String]
groups  = splitOn "\n\n"

processGroupPart2 :: String -> Int
processGroupPart2 g = res where
    -- make  set per line then intersect them 
    (s:ss) = map fromList  $ lines g
    res  = length $ foldr intersection s ss
    
processGroupPart1 :: String -> Int
processGroupPart1 g = res where
    -- unique in the whole group
    res = length . nub . foldr (:) "" . concat .  words $ g 

day6Main :: IO ()
day6Main = do
    inp <- readFile "data/Day6Part1.txt" 
    let p1 =sum . map processGroupPart1 . groups $ inp 
    print p1
    let p2 =sum . map processGroupPart2 . groups $ inp 
    print p2