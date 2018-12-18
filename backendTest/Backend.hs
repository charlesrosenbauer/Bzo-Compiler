module Main where
import System.IO
import System.Process
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
  | Set     Int   Int










printRets :: [Int] -> String
printRets qs = (printInter ", " qs) ++ " := "










printPars :: [Int] -> String
printPars is = "(" ++ (printInter ", " is) ++ ")"










printInter :: String -> [Int] -> String
printInter str xs = (concat $ intersperse str (map printVar xs))










printDef :: (Int, Int) -> String
printDef (v, t) = (printVar v) ++ " " ++ (printType t)










printExpr :: Expression -> String
printExpr (Add   q is) = (printVar q) ++ " := " ++ (printInter "+" is) ++ "\n"
printExpr (Sub   q is) = (printVar q) ++ " := " ++ (printInter "-" is) ++ "\n"
printExpr (Mul   q is) = (printVar q) ++ " := " ++ (printInter "*" is) ++ "\n"
printExpr (Div   q is) = (printVar q) ++ " := " ++ (printInter "/" is) ++ "\n"
printExpr (Mod   q is) = (printVar q) ++ " := " ++ (printInter "%" is) ++ "\n"
printExpr (IntLit q i) = (printVar q) ++ " := int64(" ++ (show i) ++ ")\n"
printExpr (FltLit q f) = (printVar q) ++ " := float64(" ++ (show f) ++ ")\n"
printExpr (StrLit q s) = (printVar q) ++ " := \"" ++ s ++ "\"\n"
printExpr (FnCall [] fn is) = (printFunc fn) ++ (printPars is) ++ "\n"
printExpr (FnCall qs fn is) = (printRets qs) ++ (printFunc fn) ++ (printPars is) ++ "\n"
printExpr (BICall [] fn is) = fn ++ (printPars is) ++ "\n"
printExpr (BICall qs fn is) = (printRets qs) ++ fn ++ (printPars is) ++ "\n"
printExpr (Set    q i) = (printVar q) ++ " = " ++ (printVar i) ++ "\n"









printFunc :: Int -> String
printFunc 0 = "main"
printFunc x = "F" ++ (show x)










printType :: Int -> String
printType (-1) = "int8"
printType (-2) = "int16"
printType (-3) = "int32"
printType (-4) = "int64"
printType   0  = "interface{}"
printType   x  = "T" ++ (show x)










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
    "(" ++ (concatMap (\(a,b) -> (printVar a) ++ " " ++ (printType b) ++ ", ") ins) ++
  ") (" ++ (concatMap (\(a,b) -> (printVar a) ++ " " ++ (printType b) ++ ", ") exs) ++ ") {\n"

printFnHeader fid ins exs =
  "func F" ++ (show fid) ++
    "(" ++ (concatMap (\(a,b) -> (printVar a) ++ " " ++ (printType b) ++ ", ") ins) ++
  ") (" ++ (concatMap (\(a,b) -> (printVar a) ++ " " ++ (printType b) ++ ", ") exs) ++ ") {\n"










printFn :: Int -> [(Int, Int)] -> [(Int, Int)] -> [Expression] -> String
printFn fid ins exs exprs =
  printFnHeader fid ins exs ++ (concatMap printExpr exprs) ++ "return\n}\n"










printTy :: Int -> [(Int, Int)] -> String
printTy tid fields =
  "type T" ++ (show tid) ++ " struct {\n" ++
  (concat $ intersperse "\n" $ map printDef fields) ++ "\n}\n"












main :: IO ()
main = do
  writeFile "output.go" ((printHeader "main" ["fmt"]) ++
    (printTy 1 [(1, -1), (2, -2)]) ++
    (printFn 0 [] [] [(IntLit 1 1), (IntLit 2 2), (FnCall [3] 1 [1, 2]), (StrLit 4 "1 + 2 ="), (BICall [] "fmt.Println" [4, 3])]) ++
    (printFn 1 [(1, -4), (2, -4)] [(3, -4)] [(Add 4 [1, 2]), (Set 3 4)]))
  callCommand "go build output.go"
