module BzoTypeBackend where
import BzoTypes
import HigherOrder
import Data.Int
import System.IO
import Debug.Trace









--TODO: Add case for type classes, maybe?
-- Not 100% sure we even need to account for type classes in the backend.
printTDef :: Int64 -> Definition -> String
printTDef i (TypeDef _ _ _ _ ty) = "\ntype T" ++ (show i) ++ " " ++ (printType ty) ++ "\n"
printTDef _ _ = ""










printType :: Type -> String
-- I'm not 100% sure about these first two cases, but we'll see how things go.
printType (PolyType _ [x, (VoidType _)]) = "*" ++ (printType x)
printType (PolyType _ [(VoidType _), x]) = "*" ++ (printType x)

printType (PolyType _  _) = "interface{}"
printType (CmpdType _ ts) =
  let fieldnames = map (\i -> "V" ++ (show i) ++ " ") [0..]
      tys        = map printType ts
      fields     = map (++ ",\n") $ zipWith (++) fieldnames tys
  in "struct{\n" ++ (concat fields) ++ "}\n"

printType (ArryType _  0 t) = "[]" ++ (printType t)
printType (ArryType _ sz t) = "[" ++ (show sz) ++ "]" ++ (printType t)
printType (FuncType _ i  o) = "func (" ++ (show i) ++ ")(" ++ (show o) ++ ")"
printType (LtrlType _    t) = "T" ++ (show t) ++ " "
printType (IntType  _    i) = "int64 "
printType (FltType  _    f) = "float64 "
printType (StrType  _    s) = "string "
printType _ = " //<???>"

{-
  TODO:
  * Handle builtin types
  * Figure out what exactly to do with tvars (probably nothing)
-}









printHeader :: String -> [String] -> String
printHeader mname imports =
  "package " ++ mname ++ "\n\n" ++
  "import (\n" ++ (concatMap (\x -> "\"" ++ x ++ "\"\n") imports) ++ ")\n\n"










printTypes :: String -> [(Int64, Definition)] -> IO()
printTypes file defs = do
  writeFile file ((printHeader "types" ["fmt"]) ++ (concatMap (\(i,d) -> printTDef i d) defs))
