module BzoChecker where
import BzoTypes
import SymbolTable
import qualified Data.Text   as T
import qualified Data.Either as E
import Debug.Trace










data DefState
  = DefState {
      ds_defs    :: [Definition T.Text],
      ds_errs    :: [BzoErr] }










makeRecordDef :: ModelRecord -> Definition T.Text
makeRecordDef (ModelRecord p nm pr ty) = (RcDefinition (ty, (T.pack pr), p) (T.pack nm))










makeEnumDef :: ModelEnum -> Definition T.Text
makeEnumDef (ModelEnum p nm pr ty) = (EnDefinition (ty, (T.pack pr), p) (T.pack nm))










defOrganizer :: DefState -> CallAST -> DefState
defOrganizer (DefState [] errs) t@(CA_TypeDefCall p i prs rs es df) =
  (DefState ([TyDefinition (prs, df, rs, es, p) (T.pack i)] ++ (map makeRecordDef rs) ++ (map makeEnumDef es)) errs)

defOrganizer (DefState (d:ds) errs) t@(CA_TypeDefCall p i prs rs es df) =
  (DefState ([TyDefinition (prs, df, rs, es, p) (T.pack i)] ++ (map makeRecordDef rs) ++ (map makeEnumDef es) ++ ds) errs)

defOrganizer (DefState [] errs) t@(CA_FTDefCall p i it xt) =
  (DefState [FnDefinition [(it, xt, p)] [] (T.pack i)] errs)

defOrganizer (DefState (d:ds) errs) t@(CA_FTDefCall p i it xt) =
  (DefState (FnDefinition [(it, xt, p)] [] (T.pack i):ds) errs)

defOrganizer (DefState [] errs) t@(CA_FnDefCall p i ip xp df) =
  (DefState [] ((SntxErr (ca_pos t) "Isolated Function Definition"):errs))

defOrganizer (DefState (d:ds) errs) t@(CA_FnDefCall p i ip xp df) =
  (DefState (FnDefinition [] [(ip, xp, df, p)] (T.pack i):ds) errs)

-- Add Hint Calls Too!

defOrganizer (DefState ds errs) t = (DefState ds ((SntxErr (ca_pos t) $ "Unexpected Definition Order!" ++ (show ds)):errs))










defOrganizePass :: BzoFileModel CallAST -> Either [BzoErr] (BzoFileModel [Definition T.Text])
defOrganizePass (BzoFileModel mn fp dm (CA_Calls _ model) fi fl ia la) =
  case (foldl defOrganizer (DefState [] []) model) of
    (DefState model' []) -> Right (BzoFileModel mn fp dm model' fi fl ia la)
    (DefState _      er) -> Left er










wrappedDefOrganizePass :: [BzoFileModel CallAST] -> Either [BzoErr] [BzoFileModel [Definition T.Text]]
wrappedDefOrganizePass xs =
  let xs' = map defOrganizePass xs
      xsl = concat $ E.lefts  xs'
      xsr = E.rights xs'
  in case xsl of
      [] -> Right xsr
      er -> Left  $ reverse xsl
