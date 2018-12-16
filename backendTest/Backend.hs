module Main where
import System.IO










data Expression
  = FnCall [Int]  Int  [Int]
  | Add     Int  [Int]
  | Sub     Int  [Int]
  | Mul     Int  [Int]
  | Div     Int  [Int]
  | Mod     Int  [Int]
  | IntLit  Int   Int
  | FltLit  Int   Double










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
    "(" ++ (concatMap (\(a,b) -> "V" ++ (show a) ++ " T" ++ (show b)) ins) ++
  ") (" ++ (concatMap (\(a,b) -> "V" ++ (show a) ++ " T" ++ (show b)) exs) ++ ") {"

printFnHeader fid ins exs =
  "func F" ++ (show fid) ++
    "(" ++ (concatMap (\(a,b) -> "V" ++ (show a) ++ " T" ++ (show b)) ins) ++
  ") (" ++ (concatMap (\(a,b) -> "V" ++ (show a) ++ " T" ++ (show b)) exs) ++ ") {"










-- Later, add contents parameter
printFn :: Int -> [(Int, Int)] -> [(Int, Int)] -> String
printFn fid ins exs =
  printFnHeader fid ins exs ++ "\nfmt.Println(\"Hello World\")\n}"






main :: IO ()
main =
  writeFile "output.go" ((printHeader "main" ["fmt"]) ++ (printFn 0 [] []))
