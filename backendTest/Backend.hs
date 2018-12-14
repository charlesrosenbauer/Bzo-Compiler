module Main where
import System.IO










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
