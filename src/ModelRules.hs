module ModelRules where
import BzoTypes
import HigherOrder
import Data.Either
import Data.Maybe
import Data.Text
import Data.List as L
import SymbolTable
import Debug.Trace









{-
wrappedModellerMap :: [BzoFileModel BzoSyntax] -> Either [BzoErr] [BzoFileModel CallAST]
wrappedModellerMap files =
  case (foldl modelFiles ([], files)) of
    ([] , files') -> Right files'
    (er , _     ) -> Left  ers










modelFiles :: SymbolTable -> ([BzoErr], [BzoFileModel BzoSyntax], [BzoFileModel BzoSyntax]) -> ([BzoErr], [BzoFileModel BzoSyntax])
modelFiles syms (errs, (BzoFileModel mn path dom model imps lnks impas lnkas):files) files' =
  let (errs, defs) =
  in
-}









modelFile :: String -> String -> ([BzoErr], [BzoSyntax], SymbolTable) -> BzoSyntax -> ([BzoErr], [BzoSyntax], SymbolTable)
modelFile mn path ([], [], syms) (BzS_Calls p calls) =
  let (errs, defs, syms') = L.foldl (modelFile mn path) ([], [], syms) calls   -- Was map, switched to foldl to handle symbol table stuff. Fix this!!
      syms'' = addFile syms' $ pack mn
  in (errs, defs, syms'')

modelFile mn path (errs, defs, syms) fn@(BzS_FunDef p ins fnid exs def) =
  let syms' = addSymbol (pack mn) syms (pack fnid)
  -- Lambda Extraction
      (lmas, fn') = extractLambda mn path fn
      syms''      = L.foldl (modelFile mn path) ([], [], syms') lmas
      defs'       = [fn'] ++ lmas ++ defs
  in (errs, defs', syms')

modelFile mn path (errs, defs, syms) ft@(BzS_FnTypeDef p pars fnid def) =
  let syms' = addSymbol (pack mn) syms (pack fnid)
  in (errs, ft:defs, syms')

modelFile mn path (errs, defs, syms) ty@(BzS_TypDef p pars tyid def) =
  let syms' = addSymbol (pack mn) syms (pack tyid)
  -- Add Enum and Record Extraction
  in (errs, ty:defs, syms')

modelFile mn path (errs, defs, syms) other = (errs, other:defs, syms)










-- | Returns all found lambdas, as well as a new definition for the function with renamed lambdas
extractLambda :: String -> String -> BzoSyntax -> ([BzoSyntax], BzoSyntax)
extractLambda mn path (BzS_Cmpd      p xs) = (\(a, b) -> (L.concat a, BzS_Cmpd  p b)) $ unzip $ L.map (extractLambda mn path) xs
extractLambda mn path (BzS_Poly      p xs) = (\(a, b) -> (L.concat a, BzS_Poly  p b)) $ unzip $ L.map (extractLambda mn path) xs
extractLambda mn path (BzS_Expr      p xs) = (\(a, b) -> (L.concat a, BzS_Expr  p b)) $ unzip $ L.map (extractLambda mn path) xs
extractLambda mn path (BzS_Block     p xs) = (\(a, b) -> (L.concat a, BzS_Block p b)) $ unzip $ L.map (extractLambda mn path) xs

extractLambda mn path (BzS_Statement p x  ) = (\(a, b) -> (a, BzS_Statement    p b  )) $ extractLambda mn path x
extractLambda mn path (BzS_Box       p x  ) = (\(a, b) -> (a, BzS_Box          p b  )) $ extractLambda mn path x
extractLambda mn path (BzS_ArrayObj  p x s) = (\(a, b) -> (a, BzS_ArrayObj     p b s)) $ extractLambda mn path x
extractLambda mn path (BzS_MapObj    p x  ) = (\(a, b) -> (a, BzS_MapObj       p b  )) $ extractLambda mn path x
extractLambda mn path (BzS_CurryObj  p x c) = (\(a, b) -> (a, BzS_CurryObj     p b c)) $ extractLambda mn path x

extractLambda mn path (BzS_FilterObj p x   f) = (\(a, b) (c, d) -> (a ++ L.concat c, BzS_FilterObj p b d)) (extractLambda mn path x) (unzip $ L.map (extractLambda mn path) f)
extractLambda mn path (BzS_FnTy      p i   o) = (\(a, b) (c, d) -> (a ++ c,          BzS_FnTy      p b d)) (extractLambda mn path i) (extractLambda mn path o)
extractLambda mn path (BzS_FnHint    p i h o) = (\(a, b) (c, d) -> (a ++ c,          BzS_FnHint  p b h d)) (extractLambda mn path i) (extractLambda mn path o)
extractLambda mn path (BzS_TyHint    p i h o) = (\(a, b) (c, d) -> (a ++ c,          BzS_TyHint  p b h d)) (extractLambda mn path i) (extractLambda mn path o)

extractLambda mn path (BzS_Lambda p pars def) =
  let (plmas, pars') = extractLambda mn path pars
      (dlmas, def' ) = extractLambda mn path pars
      lmid           = ";" ++ mn ++ ";" ++ path ++ ";" ++ (show $ line p) ++ ";" ++ (show $ column p)
      lma            = BzS_FunDef p pars lmid BzS_Undefined def
  in  ([lma] ++ plmas ++ dlmas, BzS_Id p lmid)

extractLambda mn path x = ([], x)
