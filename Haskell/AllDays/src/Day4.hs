module Day4 where

import Data.List.Split ( splitOn )
-- Passport is contained in a block
-- Block is at least one line and there
-- might be \n within a line 
-- get all blocks, these are separated by \n\n

-- for each block apply words function
--    
type Key = String
type Val = String
type Pair = (String, String)

-- splits on \n\n 
passPortBlocks :: String  -> [String]
passPortBlocks  = splitOn "\n\n"

passports :: [String] -> [[String]]
passports  = map words 

asPairs :: [[String]] -> [[Pair]]
asPairs  = (map . map) pair  where
   pair str = let [a,b] = splitOn ":" str in (a, b) 
     
-- collect all the keys and ignore pid 
checkPassportKeys :: [Pair] -> Bool
checkPassportKeys kvs = ok where
    keys = map fst kvs
    ok
        |  "cid" `elem` keys = length keys == 8
        | otherwise = length keys == 7


passportsKeysOK :: [[Pair]] -> Int
passportsKeysOK p = res where 
  res = length . filter (== True) .  map   checkPassportKeys $ p


day4Main :: IO ()
day4Main = do 
    inp <- readFile "data/Day4Part1.txt" 
    let allPassportPairs = asPairs . passports . passPortBlocks $ inp
    print "Part 1."
    print (passportsKeysOK allPassportPairs)