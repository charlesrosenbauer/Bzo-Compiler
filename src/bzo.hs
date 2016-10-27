module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO hiding (try)
import BzoParser
import BzoTypes










main :: IO()
main = do
    mainLoop_ (== "quit") (readPrompt "Input: ") (\s -> (putStrLn $ parseInput s))










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










parseInput :: String -> String
parseInput input = case parse parseExpr "Bzo" input of
    Left  err -> "Error: " ++ show err
    Right val -> showTokens val

















