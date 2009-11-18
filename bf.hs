import System.Environment
import Array
import IO
import Data.Char

data InterpreterState = InterpreterState { 
                          mem    :: Array Int Int
                        , ptr    :: Int
                        , prog   :: String
                        , inst   :: String
                        , loop   :: [String] 
                        , input  :: Bool
                        , output :: Maybe Char
                        } deriving (Show)

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
findRight s i = if head s == ']' && i == 0
                   then s
                   else case head s of 
                            ']' -> findRight (tail s) (i-1)
                            '[' -> findRight (tail s) (i+1)
                            _   -> findRight (tail s) i

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

execute code = do
        let i = InterpreterState { 
                    mem = array (1,10000) [(i,0) | i <- [1..10000]]
                    , ptr = 1000
                    , prog = code
                    , inst = code
                    , loop = []
                    , input = False
                    , output = Nothing
                    }
        run i

run i = do
     let newi = interpret i
     newi <- handleIO newi
     if null (tail (inst newi))
        then return ()
        else run $ newi { inst = tail (inst newi) }



main = do
    filename <- getArgs
    file <- openFile (head filename) ReadMode
    code <- (hGetContents file)
    hSetBuffering stdin NoBuffering
    execute code

