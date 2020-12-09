module Day9 where
import Data.List
import Data.Maybe

type Numbers = [Int]
type Window = Int

numberOk ::  Numbers -> Window -> Int -> Bool
numberOk  nums w n 
    | ix <= w =  True
    | otherwise = pairs /= [] where 
            sublist = drop (ix - w) (take ix nums)
            pairs = [(x, y) | x <-sublist, y<-sublist, x + y == n && x /= y] 
            ix = fromMaybe 0 (elemIndex n  nums)

--  first failure
part1::Numbers -> Window -> Int
part1 nums w = res where
    (_, res) = head 
                . dropWhile fst 
                . map (\x -> (numberOk nums w x, x )) 
                . drop w $ nums
   

day9Main :: IO ()
day9Main = do 
    inp <- readFile "../data/Day9Part1.txt" 
    let nums = map (\s -> read s::Int) $ lines inp
    
    print (part1 nums 25)