module Main where
import System.Environment
import System.IO
import BzoTypes
import Compiler
import Error
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
  putStrLn "This compiler is licensed under GPLv.3\n"
  putStrLn "Enter \"#quit\" to exit\n"
  putStrLn "Call the compiler with -help for help."
  return ()










printHelp :: IO()
printHelp = do
  putStrLn "Bzo is very much a work in progress.\n"
  putStrLn "If the compiler complains of an undefined environment, you need to provide it a path to a valid Bzo environment."
  putStrLn "The environment (the standard library) can be downloaded at https://github.com/charlesrosenbauer/Bzo-Standard-Library."
  putStrLn "Then, provide the compiler with the path using the -env= flag.\n"
  putStrLn "For multiline REPL entries, enter #< to start multiline entry, and #> to exit.\n"
  putStrLn "REPL currently is more of RPPL; Read, Parse, Print Loop. What is printed is the output of the parser, though it does not do as much parsing as the full compiler.\n"
  return()










main :: IO()
main = do
    args <- getArgs
    case parseParameters args of
      Left  (ParamErr                         err) -> putStrLn $ unpack err
      Right (BzoSettings  []  []  [] Opt_None  []) -> do printIntro; (mainLoop (\s -> (putStrLn $ replExpression ("REPL", ((pack s) `append` (pack "\n"))))))
      Right (BzoSettings  []  []  [Flag_HelpMePlease] _ []) -> do printHelp; return ()
      Right settings                               -> compileFilePass settings
      _                                            -> putStrLn "This shouldn't happen, but it stops a compiler warning."










mainLoop :: (String -> IO()) -> IO ()
mainLoop action = do
  line <- readPrompt "\nBzo>>>"
  case line of
    "#quit" -> return ()
    "#<"    -> (multilinePrompt "" "#>") >>= action >> mainLoop action
    input   -> (action input           )            >> mainLoop action










multilinePrompt :: String -> String -> IO String
multilinePrompt totaltxt exittxt = do
  line <- getLine
  if (line == exittxt)
    then return totaltxt
    else multilinePrompt (totaltxt ++ line ++ "\n") exittxt










readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine










flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout










printMany :: [String] -> IO ()
printMany (x:xs) = do
    putStrLn (x ++ "\n")
    printMany xs
printMany ([]) = return ()
