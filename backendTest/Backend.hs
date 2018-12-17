module Main where
import System.IO
import Data.List










data Expression
  = FnCall [Int]  Int  [Int]
  | Add     Int  [Int]
  | Sub     Int  [Int]
  | Mul     Int  [Int]
  | Div     Int  [Int]
  | Mod     Int  [Int]
  | IntLit  Int   Int
  | FltLit  Int   Double
  | StrLit  Int   String
  | BICall [Int] String [Int]










printRets :: [Int] -> String
printRets qs = (printInter ", " qs) ++ " := "










printPars :: [Int] -> String
printPars is = "(" ++ (printInter ", " is) ++ ")"










printInter :: String -> [Int] -> String
printInter str xs = (concat $ intersperse str (map printVar xs))










printExpr :: Expression -> String
printExpr (Add   q is) = (printVar q) ++ " := " ++ (printInter "+" is) ++ "\n"
printExpr (Sub   q is) = (printVar q) ++ " := " ++ (printInter "-" is) ++ "\n"
printExpr (Mul   q is) = (printVar q) ++ " := " ++ (printInter "*" is) ++ "\n"
printExpr (Div   q is) = (printVar q) ++ " := " ++ (printInter "/" is) ++ "\n"
printExpr (Mod   q is) = (printVar q) ++ " := " ++ (printInter "%" is) ++ "\n"
printExpr (IntLit q i) = (printVar q) ++ " := " ++ (show i) ++ "\n"
printExpr (FltLit q f) = (printVar q) ++ " := " ++ (show f) ++ "\n"
printExpr (StrLit q s) = (printVar q) ++ " := \"" ++ s ++ "\"\n"
printExpr (FnCall qs fn is) = (printRets qs) ++ (printFunc fn) ++ (printPars is) ++ ")"
printExpr (BICall [] fn is) = fn ++ (printPars is) ++ "\n"
printExpr (BICall qs fn is) = (printRets qs) ++ fn ++ (printPars is) ++ "\n"









printFunc :: Int -> String
printFunc 0 = "main"
printFunc x = "F" ++ (show x)










printType :: Int -> String
printType 0 = "interface{}"
printType x = "T" ++ (show x)










printVar  :: Int -> String
printVar 0 = "_"
printVar x = "v" ++ (show x)










printHeader :: String -> [String] -> String
printHeader mname imports =
  "package " ++ mname ++ "\n\n" ++
  "import (\n" ++ (concatMap (\x -> "\"" ++ x ++ "\"\n") imports) ++ ")\n\n"










printFnHeader :: Int -> [(Int, Int)] -> [(Int, Int)] -> String
printFnHeader 0 ins exs =
  "func main" ++
    "(" ++ (concatMap (\(a,b) -> (printVar a) ++ " " ++ (printType b)) ins) ++
  ") (" ++ (concatMap (\(a,b) -> (printVar a) ++ " " ++ (printType b)) exs) ++ ") {\n"

printFnHeader fid ins exs =
  "func F" ++ (show fid) ++
    "(" ++ (concatMap (\(a,b) -> (printVar a) ++ " " ++ (printType b)) ins) ++
  ") (" ++ (concatMap (\(a,b) -> (printVar a) ++ " " ++ (printType b)) exs) ++ ") {\n"










-- Later, add contents parameter
printFn :: Int -> [(Int, Int)] -> [(Int, Int)] -> [Expression] -> String
printFn fid ins exs exprs =
  printFnHeader fid ins exs ++ (concatMap printExpr exprs) ++ "}\n"






main :: IO ()
main =
  writeFile "output.go" ((printHeader "main" ["fmt"]) ++ (printFn 0 [] [] [(IntLit 1 1), (IntLit 2 2), (IntLit 3 3), (Add 4 [1, 2, 3]), (StrLit 5 "1 + 2 + 3 = "), (BICall [] "fmt.Println" [5, 4])]))
