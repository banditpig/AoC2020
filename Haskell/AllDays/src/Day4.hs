module Day4 where
import Data.Either
import Data.List.Split ( splitOn )
import Validation
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
     
-- collect all the keys and ignore cid 
checkPassportKeys :: [Pair] -> Bool
checkPassportKeys kvs = ok where
    keys = map fst kvs
    ok
        |  "cid" `elem` keys = length keys == 8
        | otherwise = length keys == 7


checkPassportValues :: [Pair] -> Bool
checkPassportValues kvs = ok where 
   -- vals = map snd kvs
   -- fold the values togive a list of validation funcs
   results = concat $ foldl (\acc (l,r) -> [validatorDispatch l r]:acc ) [] kvs
   ok = filter (\x ->  isLeft x ) results == []
   
allActualPassportValues ps = res where
  oneVal kvs = concat $ foldl (\acc (l,r) -> [validatorDispatch l r]:acc ) [] kvs
  res =map oneVal ps

passportsKeysOK :: [[Pair]] -> Int
passportsKeysOK p = res where 
  res = length . filter (== True) .  map   checkPassportKeys $ p

passportsWithKeysOK :: [[Pair]] -> [[Pair]]
passportsWithKeysOK ps = res where 
  res = foldl (\acc pp -> if checkPassportKeys(pp) then pp :acc else acc) [] ps

passportsValuesOK :: [[Pair]] -> Int
passportsValuesOK p = res  where  
  res = length . filter (== True) .  map   checkPassportValues $ p
-- ---------------------------------------
validatorDispatch str = 
  case str of
    "byr" -> birthCheck
    "iyr" -> issueCheck
    "eyr" -> expireCheck
    "hgt" -> heightCheck
    "hcl" -> hairCheck
    "ecl" -> eyeCheck
    "pid" -> pidCheck
    "cid" -> alwaysRight


-- ---------------------------------------

day4Main :: IO ()
day4Main = do 
    inp <- readFile "data/Day4Part1.txt" 
    let allPassportPairs = asPairs . passports . passPortBlocks $ inp
    print "Part 1."
    let withKeysOK = passportsKeysOK allPassportPairs
    print withKeysOK

    print ""
    print $ passportsValuesOK (passportsWithKeysOK allPassportPairs)
    -- let withValsOK = passportsValuesOK withKeysOK
    
    -- print $ allActualPassportValues (passportsWithKeysOK allPassportPairs)
