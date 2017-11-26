module Main where
import System.IO
import IRLexer
import IRParser










main :: IO()
main = do
  fcontents <- readFile "test.bzir"
  putStrLn (fcontents ++ "\n\n" ++ (show $ eitherMap (irParse "test.bzir") $ lexFile "test.bzir" fcontents))






eitherMap :: (b -> c) -> Either a b -> Either a c
eitherMap f (Right b) = Right $ f b
eitherMap f (Left  a) = Left  a
