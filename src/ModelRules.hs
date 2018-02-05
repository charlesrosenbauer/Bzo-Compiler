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
  -- Add Lambda Extraction
  in (errs, fn:defs, syms')

modelFile mn path (errs, defs, syms) ft@(BzS_FnTypeDef p fnid def) =
  let syms' = addSymbol (pack mn) syms (pack fnid)
  in (errs, ft:defs, syms')

modelFile mn path (errs, defs, syms) ty@(BzS_TypDef p pars tyid def) =
  let syms' = addSymbol (pack mn) syms (pack tyid)
  -- Add Enum and Record Extraction
  in (errs, ty:defs, syms')

modelFile mn path (errs, defs, syms) other = (errs, other:defs, syms)
