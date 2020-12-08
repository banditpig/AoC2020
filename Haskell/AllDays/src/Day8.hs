{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
{-# OPTIONS -fwarn-typed-holes #-}

module Day8 where

import Data.List.Split 
import Data.List.Utils (replace)

data Op =
    Nop Int | 
    Acc Int | 
    Jmp Int  
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
        _     -> Nop 0 -- stop compiler complaining
        where [op, n] = splitOn " " . clean $ s

clean :: String -> String
clean  = replace "+" "" 

parseInput :: [String] -> Program
parseInput ss = (map parseOp ss, 0, 0, [])  

evalNextOp :: Program -> Program
evalNextOp (ops, acc, pc, used) = 
   let op  = (!!) ops  pc in 
        if (op, pc) `elem` used then
            (ops, acc, -1, used) 
            else
        case  op of
            Jmp  i -> (ops, acc, pc + i, (op, pc):used)
            Nop  _ -> (ops, acc, pc + 1, (op, pc):used)
            Acc  i -> (ops, acc + i, pc + 1, (op, pc):used)    

execute :: Program -> Program
execute p@(_, _, -1 , _) = p -- part 1 uses -1 to indicate termination
execute p =  execute $ evalNextOp p


day8Main :: IO ()
day8Main = do
    inp <- readFile "../data/Day8Part1.txt" 
    let code = lines inp
    print code
    print ""
    let (_, acc, _, _ ) = execute (parseInput code)
    print acc