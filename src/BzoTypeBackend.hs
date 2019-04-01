module BzoTypeBackend where
import BzoTypes
import HigherOrder
import Data.Int
import System.IO
import Debug.Trace










printTLit :: Int64 -> String
printTLit  1 = "int8 "
printTLit  2 = "int16 "
printTLit  3 = "int32 "
printTLit  4 = "int64 "
printTLit  5 = "uint8 "
printTLit  6 = "uint16 "
printTLit  7 = "uint32 "
printTLit  8 = "uint64 "
printTLit  9 = "float32 "  -- F16
printTLit 10 = "float32 "
printTLit 11 = "float64 "
printTLit 12 = "float32 "  -- P8
printTLit 13 = "float32 "  -- P16
printTLit 14 = "float32 "  -- P32
printTLit 15 = "float64 "  -- P64
printTLit 16 = "string "
printTLit 17 = "rune "
printTLit 18 = "rune "
printTLit 19 = "rune "
printTLit 20 = "bool "
printTLit 21 = "uint8 "
printTLit 22 = "uint16 "
printTLit 23 = "uint32 "
printTLit 24 = "uint64 "
printTLit 25 = "[2]uint64 "
printTLit 26 = "[4]uint64 "
printTLit 27 = "[8]uint64 "
printTLit 28 = "Regexp"
printTLit  x = "T" ++ (show x) ++ " "










--TODO: Add case for type classes, maybe?
-- Not 100% sure we even need to account for type classes in the backend.
printTDef :: Int64 -> Definition -> String
printTDef i (TypeDef tid _ _ _ ty) = "\n/*" ++ (show tid) ++ "\n" ++ (show ty) ++ "*/\ntype T" ++ (show i) ++ " " ++ (printType ty) ++ "\n"
printTDef _ _ = ""










printType :: Type -> String
-- I'm not 100% sure about these first two cases, but we'll see how things go.
printType (PolyType _ [x, (VoidType _)]) = "*" ++ (printType x)
printType (PolyType _ [(VoidType _), x]) = "*" ++ (printType x)

printType (PolyType _  _) = "interface{} "
printType (CmpdType _ ts) =
  let fieldnames = map (\i -> "V" ++ (show i) ++ " ") [0..]
      tys        = map printType ts
      fields     = map (++ ",\n") $ zipWith (++) fieldnames tys
  in "struct{\n" ++ (concat fields) ++ "}\n"

printType (ArryType _  0 t) = "[]" ++ (printType t)
printType (ArryType _ sz t) = "[" ++ (show sz) ++ "]" ++ (printType t)
printType (FuncType _ i  o) = "func (" ++ (show i) ++ ")(" ++ (show o) ++ ")"
printType (LtrlType _    t) = printTLit t
printType (BITyType _    t) = printTLit t
printType (TVarType _    _) = "interface{} "
printType (IntType  _    i) = "int64 "
printType (FltType  _    f) = "float64 "
printType (StrType  _    s) = "string "
printType (VoidType _     ) = "bool "  -- Until I find a better type to use.
printType (MakeType _   ts) = printType $ head ts
printType t = " /*" ++ (show t) ++ "*/"

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
  writeFile file ((printHeader "types" ["fmt", "regexp"]) ++ (concatMap (\(i,d) -> printTDef i d) defs))
