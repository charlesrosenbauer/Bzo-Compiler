module Main where
import System.Environment
import System.IO
import BzoTypes
import Compiler
import BzoParameterParser










main :: IO()
main = do
    args <- getArgs
    case parseParameters args of
      Left  (ParamErr                         err) -> putStrLn err
      Right (BzoSettings  []  []  [] Opt_None  []) -> mainLoop_ (== "$quit") (readPrompt "\nBzo>>> ") (\s -> (putStrLn $ replExpression ("REPL", (s ++ "\n"))))
      Right settings                               -> compileFilePass settings
      _                                            -> putStrLn "This shouldn't happen, but it stops a compiler warning."










mainLoop_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m()
mainLoop_ endCond prompt action = do
    result <- prompt
    if endCond result
        then return ()
        else action result >> mainLoop_ endCond prompt action










readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine










flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout










printMany :: [String] -> IO ()
printMany (x:xs) = do
    putStrLn (x ++ "\n")
    printMany xs
printMany ([]) = return ()
