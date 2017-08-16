module Main where
import Control.Monad
import System.Environment
import System.IO
import BzoLexer
import BzoTypes
import Compiler
import BzoParameterParser
--import BzoError










main :: IO()
main = do
    args <- getArgs
    case parseParameters args of
      Left  (ParamErr                         err) -> putStrLn err
      Right (BzoSettings  []  []  [] Opt_None  []) -> mainLoop_ (== "$quit") (readPrompt "Bzo>>> ") (\s -> (putStrLn $ compileExpression ("REPL", (s ++ "\n"))))
      Right settings                               -> compileFilePass settings










mainLoop_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m()
mainLoop_ until prompt action = do
    result <- prompt
    if until result
        then return ()
        else action result >> mainLoop_ until prompt action










readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine










flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout










printMany :: [String] -> IO ()
printMany (x:xs) = do
    putStrLn (x ++ "\n")
    printMany xs
printMany ([]) = return ()
