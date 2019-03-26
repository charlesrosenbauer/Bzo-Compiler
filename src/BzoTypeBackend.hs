module BzoTypeBackend where
import BzoTypes
import HigherOrder
import System.IO
import Debug.Trace









--TODO: Add case for type classes, maybe?
-- Not 100% sure we even need to account for type classes in the backend.
printTDef :: Int -> Definition -> String
printTDef i (TypeDef _ _ _ _ ty) = "\ntype T" ++ (show i) ++ (printType ty) ++ "\n"
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

{-
  TODO:
  * Add Function types
  * Handle Literal cases
  * Figure out what exactly to do with tvars (probably nothing)
-}
