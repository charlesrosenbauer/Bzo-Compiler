module Main where
import System.Environment
import System.IO
import BzoTypes
import Compiler
import BzoParameterParser
import Data.Text










printIntro :: IO()
printIntro = do
  putStrLn "\n88888888ba   888888888888    ,ad8888ba,   "
  putStrLn "88      \"8b           ,88   d8\"'    `\"8b  "
  putStrLn "88      ,8P         ,88\"   d8'        `8b  "
  putStrLn "88aaaaaa8P'       ,88\"     88          88  "
  putStrLn "88\"\"\"\"\"\"8b,     ,88\"       88          88  "
  putStrLn "88      `8b   ,88\"         Y8,        ,8P  "
  putStrLn "88      a8P  88\"            Y8a.    .a8P   "
  putStrLn "88888888P\"   888888888888    `\"Y8888Y\"'\n"
  putStrLn "Created by Charles Rosenbauer (https://github.com/charlesrosenbauer)"
  putStrLn "Github for Compiler: https://github.com/charlesrosenbauer/Bzo-Compiler"
  putStrLn "Note: This is a work in progress. Don't expect everything to work right now."
  putStrLn "This compiler is licensed under GPLv.3"
  putStrLn "Enter \"#quit\" to exit\n"
  return ()










main :: IO()
main = do
    args <- getArgs
    case parseParameters args of
      Left  (ParamErr                         err) -> putStrLn $ unpack err
      Right (BzoSettings  []  []  [] Opt_None  []) -> do printIntro; (mainLoop_ (== "#quit") (readPrompt "\nBzo>>> ") (\s -> (putStrLn $ replExpression ("REPL", ((pack s) `append` (pack "\n"))))))
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
