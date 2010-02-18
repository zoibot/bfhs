import Brainfuck
import System.Environment
import IO

execute :: String -> IO ()
execute code = do
        let i = initializeInterpreter code
        run i

run :: InterpreterState -> IO ()
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
