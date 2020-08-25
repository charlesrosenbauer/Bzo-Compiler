{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2020 Charles Rosenbauer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.-}

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
  putStrLn "Bzo Bootstrapping Compiler  Copyright (C) 2020  Charles Rosenbauer"
  putStrLn "This program comes with ABSOLUTELY NO WARRANTY"
  putStrLn "This is free software, and you are welcome to redistribute it"
  putStrLn "For license and warranty info, enter \"#license\""
  putStrLn "For help, enter \"#help\""
  return ()










printLicense :: IO()
printLicense = do
  putStrLn "This is the bootstrapping compiler for the Bzo programming language."
  putStrLn "Copyright (C) 2020 Charles Rosenbauer"

  putStrLn "This program is free software: you can redistribute it and/or modify"
  putStrLn "it under the terms of the GNU General Public License as published by"
  putStrLn "the Free Software Foundation, either version 3 of the License, or"
  putStrLn "(at your option) any later version."

  putStrLn "This program is distributed in the hope that it will be useful,"
  putStrLn "but WITHOUT ANY WARRANTY; without even the implied warranty of"
  putStrLn "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
  putStrLn "GNU General Public License for more details."

  putStrLn "You should have received a copy of the GNU General Public License"
  putStrLn "along with this program.  If not, see <https://www.gnu.org/licenses/>."










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
    "#help"   -> printHelp    >> mainLoop action
    "#license"-> printLicense >> mainLoop action
    "#quit"   -> return ()
    "#<"      -> (multilinePrompt "" "#>") >>= action >> mainLoop action
    input     -> (action input           )            >> mainLoop action










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
