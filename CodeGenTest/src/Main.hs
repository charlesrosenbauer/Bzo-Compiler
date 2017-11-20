module Main where
import System.IO
import IRLexer










main :: IO()
main = do
  fcontents <- readFile "test.bzir"
  putStrLn (fcontents ++ "\n\n" ++ (show $ lexFile "test.bzir" fcontents))
