module Main where
import System.IO
import IRLexer
import IRParser
import IRModel
import qualified Data.Map.Strict as M










main :: IO()
main = do
  fcontents <- readFile "test.bzir"
  putStrLn (fcontents ++ "\n\n" ++ (
    case (eitherWraps modelIR $ eitherWrap (irParse "test.bzir") $ lexFile "test.bzir" fcontents) of
      Right (syms, fs, ts, cs, hs) -> "Success:\n----------\n"   ++ (show syms) ++
                                          "\n\n  Functions:\n" ++ (concatMap (\x -> "    " ++ (show x) ++ "\n\n") $ M.assocs fs) ++
                                          "\n\n  Types:\n"     ++ (concatMap (\x -> "    " ++ (show x) ++ "\n\n") $ M.assocs ts) ++
                                          "\n\n  Constants:\n" ++ (concatMap (\x -> "    " ++ (show x) ++ "\n\n") $ M.assocs cs) ++
                                          "\n\n  Hints:\n"     ++ (show   hs) ++ "\n\n"
      Left er  -> "Errors: \n----------\n" ++ (concat $ map (\x -> (show x) ++ "\n") er)))






eitherMap :: (b -> c) -> Either a b -> Either a c
eitherMap f (Right b) = Right $ f b
eitherMap f (Left  a) = Left  a










eitherWrap :: (b -> Either a c) -> Either a b -> Either a c
eitherWrap f (Left er) = Left er
eitherWrap f (Right x) = f x










eitherWraps :: (b -> Either [a] c) -> Either a b -> Either [a] c
eitherWraps f (Left er) = Left [er]
eitherWraps f (Right x) = f x
