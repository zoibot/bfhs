module Brainfuck where

import Array
import IO
import Data.Char

--Describes the current state of the interpreter
data InterpreterState = InterpreterState { 
                          mem    :: Array Int Int
                        , ptr    :: Int
                        , inst   :: String
                        , loop   :: [String] 
                        , input  :: Bool
                        , output :: Maybe Char
                        } deriving (Show, Eq)

initializeInterpreter :: String -> InterpreterState
initializeInterpreter code = InterpreterState { 
                          mem = array (1,20000) [(i,0) | i <- [1..20000]]
                        , ptr = 1000
                        , inst = code
                        , loop = []
                        , input = False
                        , output = Nothing
                        }

writeI :: Int -> InterpreterState -> InterpreterState
writeI val i = i { mem = mem i // [(ptr i, val)] }

readI :: InterpreterState -> Int
readI i = mem i ! ptr i

movePtr :: Int -> InterpreterState -> InterpreterState
movePtr dir i = i { ptr = ptr i + dir }

leftLoop :: InterpreterState -> InterpreterState
leftLoop i = if readI i == 0 
                then i { inst = findRight (tail (inst i)) 0 }
                else i { loop = (inst i) : loop i}

findRight :: String -> Int -> String
findRight (']':xs) 0 = ']':xs
findRight ('[':xs) i = findRight xs (i+1)
findRight (']':xs) i = findRight xs (i-1)
findRight (_:xs) i = findRight xs i
findRight [] _ = error "missing end of loop"

rightLoop :: InterpreterState -> InterpreterState
rightLoop i = if readI i /= 0
                 then i { inst = head (loop i) }
                 else i { loop = tail (loop i) } 

interpret :: InterpreterState -> InterpreterState
interpret i =
    case (head (inst i)) of
        '+' -> writeI ((readI i)+1) i
        '-' -> writeI ((readI i)-1) i
        '<' -> movePtr (-1) i
        '>' -> movePtr 1 i
        '[' -> leftLoop i
        ']' -> rightLoop i
        '.' -> i { output = Just $ chr (readI i) }  
        ',' -> i { input = True }
        _   -> i

handleIn :: InterpreterState -> IO InterpreterState
handleIn i = if (input i)
               then do x <- getChar
                       return $ writeI (ord x) $ i { input = False }
               else return i

handleOut :: InterpreterState -> IO InterpreterState
handleOut i = case (output i) of
                Just a -> do putChar a
                             return $ i { output = Nothing }
                Nothing -> return i

handleIO :: InterpreterState -> IO InterpreterState
handleIO i = handleIn i >>= handleOut

