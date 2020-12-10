module Day9 where
import Data.List
import Data.Maybe
import Data.Sort
import Control.Lens
import Control.Lens.Indexed
type Numbers = [Int]
type Window = Int

sumOverWindow ::[Int] ->  Int -> Int -> (Int, Int, Int)
sumOverWindow nums  wide lo  = res  where
    end =   min (lo + wide) (length nums)
    r1 = map (\x -> nums ^? ix x)  [lo .. end]
    res = (sum (catMaybes r1), lo, end) 
 


allWindows 1000 nums target = [(0,0,0)]
allWindows w nums target 
  | results  /= [] = results
  | otherwise = allWindows (w + 1) nums target
      where 
        sumw = map (sumOverWindow nums  w)  [0.. length nums] 
        results = filter (\(v, l, u) -> v == target) sumw

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


part2  nums bad = res where 
  [(_, lo, hi)] = allWindows 2 nums bad
  range = sort . take (hi - 1) $ drop lo nums
  res  = head range + last range

part2'  nums bad = range where 
  [(_, lo, hi)] = allWindows 2 nums bad
  range = sum . sort . take (hi - 1) $ drop lo nums

day9Main :: IO ()
day9Main = do 
    let pre = 25
    inp <- readFile "../data/Day9Part1.txt" 
    let nums = map (\s -> read s::Int) $ lines inp
    let bad  = part1 nums pre
    print bad
    -- Part 2 works with test data but not with real data. :)
    print "===="
    print (part2 nums bad) 
   