
module Day5 where
import Data.List ( (\\), sort )

type Pass = String
type Row = Int
type Col = Int
type Seat = (Row, Col)

data Range = R { 
lwr :: Int, 
upr :: Int} deriving (Show) 

b :: Range -> Range
b (R l u) = R  ( l + (u - l) `div` 2) u

f :: Range -> Range
f (R l u) = R l ( l + (u - l) `div` 2)

toNumbr :: Range -> Int
toNumbr (R l u) = head [l .. u - 1]

seatId :: Seat -> Int 
seatId (r,c) = r * 8 + c

applyInstr :: Range -> Char -> Range
applyInstr r ch  
    | ch == 'F' || ch == 'L' = f r
    | otherwise = b r

resolvePass :: Pass -> Int
resolvePass p = id  where
    row = toNumbr $ foldl applyInstr (R 0 128) $ take 7 p 
    col = toNumbr $ foldl applyInstr (R 0 8) $ drop 7 p
    id = seatId (row, col)


day5Main :: IO () 
day5Main = do
    inp <- readFile "data/Day5Part1.txt"  

    let max = maximum . map resolvePass $ lines inp
    print max 

    let ids = sort . map resolvePass $ lines inp 
    print ([head ids .. last ids] \\ ids) 
