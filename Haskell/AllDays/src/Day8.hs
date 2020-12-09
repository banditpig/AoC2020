{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
{-# OPTIONS -fwarn-typed-holes #-}

module Day8 where
import Control.Lens
import Data.List
import Data.Maybe
import Data.List.Split 
import Data.List.Utils (replace)

data Op =
    Nop Int | 
    Acc Int | 
    Jmp Int |
    Hlt     
    
    deriving (Show, Eq)       


type Acc = Int  
type PC = Int 
type Used = [(Op, PC)] 
type Program = ([Op], Acc, PC, Used)

parseOp :: String -> Op
parseOp  s =
    case op of 
        "jmp" -> Jmp (read n :: Int)
        "nop" -> Nop (read n :: Int)
        "acc" -> Acc (read n :: Int)
        "hlt" -> Hlt                    -- goes onto end of input file
        _     -> Nop 0                  -- stop compiler complaining
        where [op, n] = splitOn " " . clean $ s

clean :: String -> String
clean  = replace "+" "" 

parseInput :: [String] -> Program
parseInput ss = (map parseOp ss  , 0, 0, [])  

evalNextOp :: Program -> Program
evalNextOp (ops, acc, pc, used) = 
   let op  = (!!) ops  pc in 
        if (op, pc) `elem` used then
            (ops, acc, -1, used) 
            else
                if  op == Hlt then  (ops, acc, -1, (op, acc): used) 
                else
        case  op of
            Jmp  i -> (ops, acc, pc + i, (op, pc):used)
            Nop  _ -> (ops, acc, pc + 1, (op, pc):used)
            Acc  i -> (ops, acc + i, pc + 1, (op, pc):used) 
            Hlt    -> (ops, acc, pc, used) -- wont get used  

execute :: Program -> Program
execute p@(_, _, -1 , _) = p -- part 1 uses -1 to indicate termination
execute p =  execute $ evalNextOp p


filterJmp :: Op -> Bool
filterJmp (Jmp _) = True
filterJmp _       = False

filterNop :: Op -> Bool
filterNop (Nop _) = True
filterNop _       = False

opLocations:: Op -> [Op] -> [Int]
opLocations (Jmp _) ops  =  map (\o -> fromMaybe 0 (elemIndex o ops)) filteredOps where
    filteredOps = filter filterJmp ops
opLocations (Nop _) ops =  map (\o -> fromMaybe 0 (elemIndex o ops)) filteredOps  where
    filteredOps = filter filterNop  ops
opLocations _  _ = []  

flipJmp :: [Op] -> Int -> [Op]
flipJmp ops i = ops' where
    (Jmp x) =  ops !! i
    ops' = ops & element i .~ Nop x

flipNop :: [Op] -> Int -> [Op]
flipNop ops i = ops' where
    (Nop x) =  ops !! i
    ops' = ops & element i .~ Jmp x
    

part2 :: Program -> Int
part2 (ops, ac, pc, u ) = res where
    
    newProgsJmp = map (\ i -> (flipNop ops i, ac, pc, u)) $ opLocations (Nop 0) ops 
    newProgsNop = map (\ i -> (flipJmp ops i, ac, pc, u)) $ opLocations (Jmp 0) ops 

    (_, _, _, (_, res) : _) = head $ dropWhile (\(_, _, _, (op,a):us)  -> op /= Hlt  )$ map execute (newProgsNop ++ newProgsJmp)

day8Main :: IO ()
day8Main = do
    inp <- readFile "data/Day8Part1.txt" 
    let code = lines inp
     
    print ("Part 1")
    let prog = parseInput code
    let (ops, acc, pc, _ ) = execute prog
    print acc
    print ("Part 2")
    print (part2 prog)
