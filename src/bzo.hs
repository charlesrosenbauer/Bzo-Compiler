module Main where
import Control.Monad
import System.Environment
import System.IO hiding (try)
import BzoLexer
--import BzoParser
import BzoTypes
import BzoTokens
import Compiler
--import BzoError










main :: IO()
main = do
    args <- getArgs
    case length args of
        0 -> mainLoop_ (== "$quit") (readPrompt "Bzo>>> ") (\s -> (putStrLn $ parseInput ("REPL", (s ++ "\n"))))
        otherwise -> do
            (loadMany args) >>= (printMany . parseMany . (\ss -> zip args ss))










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










parseInput :: (String, String) -> String
parseInput (name, input) = compileFile name input










loadMany :: [FilePath] -> IO [String]
loadMany files = sequence $ map readFile files









parseMany :: [(String, String)] -> [String]
parseMany s = map parseInput s










printMany :: [String] -> IO ()
printMany (x:xs) = do
    putStrLn (x ++ "\n")
    printMany xs
printMany ([]) = return ()
