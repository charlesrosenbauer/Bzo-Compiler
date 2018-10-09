module Backend where
import System.IO










printGoHeader :: [String] -> String
printGoHeader imports = "package main\n\nimport (\n" ++ (concatMap (\x -> "\"" ++ x ++"\"\n") imports) ++ ")\n\n"







printFunction :: String -> [String] -> [String] -> String -> String
printFunction fname inpars expars contents =
  "\nfunc " ++ fname ++ "(" ++ (concatMap (++",") inpars) ++ ")" ++
                        "(" ++ (concatMap (++",") expars) ++ "){\n" ++
                        contents ++ "\n}\n"
