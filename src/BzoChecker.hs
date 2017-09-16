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

defOrganizer (DefState ds errs) t@(CA_TypeDefCall p i prs rs es df) =
  (DefState ([TyDefinition (prs, df, rs, es, p) (T.pack i)] ++ (map makeRecordDef rs) ++ (map makeEnumDef es) ++ ds) errs)

defOrganizer (DefState [] errs) t@(CA_FTDefCall p i it xt) =
  (DefState [FnDefinition [(it, xt, p)] [] (T.pack i)] errs)

defOrganizer (DefState (d:ds) errs) t@(CA_FTDefCall p i it xt) =
  case d of
    (TyDefinition   _ _ ) -> (DefState (FnDefinition [(it, xt, p)] [] (T.pack i):ds) errs)
    (RcDefinition   _ _ ) -> (DefState (FnDefinition [(it, xt, p)] [] (T.pack i):ds) errs)
    (EnDefinition   _ _ ) -> (DefState (FnDefinition [(it, xt, p)] [] (T.pack i):ds) errs)
    (HintDefinition _ _ ) -> (DefState (FnDefinition [(it, xt, p)] [] (T.pack i):ds) errs)
    (NilDefinition      ) -> (DefState (FnDefinition [(it, xt, p)] [] (T.pack i):ds) errs)

    (FnDefinition ft [] a) ->
      if (a == (T.pack i))
        then (DefState (FnDefinition ((it, xt, p):ft) [] (T.pack i):ds) errs)
        else (DefState (NilDefinition:d:ds) ((SntxErr (ca_pos t) $ "Expected Function Type Definition for " ++ i):errs))

    (FnDefinition ft fn a) -> (DefState ((FnDefinition [(it, xt, p)] [] (T.pack i)):d:ds) errs)

defOrganizer (DefState [] errs) t@(CA_FnDefCall p i ip xp df) =
  (DefState [] ((SntxErr (ca_pos t) "Isolated Function Definition"):errs))

defOrganizer (DefState (d:ds) errs) t@(CA_FnDefCall p i ip xp df) =
  case d of
    (TyDefinition   _ _ ) -> (DefState (NilDefinition:d:ds) ((SntxErr (ca_pos t) "Function Definition without Defined Type"):errs))
    (RcDefinition   _ _ ) -> (DefState (NilDefinition:d:ds) ((SntxErr (ca_pos t) "Function Definition without Defined Type"):errs))
    (EnDefinition   _ _ ) -> (DefState (NilDefinition:d:ds) ((SntxErr (ca_pos t) "Function Definition without Defined Type"):errs))
    (HintDefinition _ _ ) -> (DefState (NilDefinition:d:ds) ((SntxErr (ca_pos t) "Function Definition without Defined Type"):errs))
    (NilDefinition      ) -> (DefState (NilDefinition:d:ds) ((SntxErr (ca_pos t) "Function Definition without Defined Type"):errs))

    (FnDefinition ft [] a) ->
      if (a == (T.pack i))
        then (DefState (FnDefinition ft [(ip, xp, df, p)] (T.pack i):ds) errs)
        else (DefState (NilDefinition:d:ds) ((SntxErr (ca_pos t) $ "Expected Definition for Function " ++ i):errs))

    (FnDefinition ft fn a) ->
      if (a == (T.pack i))
        then (DefState (FnDefinition ft ((ip, xp, df, p):fn) (T.pack i):ds) errs)
        else (DefState (NilDefinition:d:ds) ((SntxErr (ca_pos t) $ "Expected Definition for Function " ++ i):errs))

defOrganizer (DefState ds errs) t@(CA_HintCall p h ps) =
  (DefState ((HintDefinition (ps, p) (T.pack h)):ds) errs)

defOrganizer (DefState ds errs) t = (DefState ds ((SntxErr (ca_pos t) $ "Unexpected Definition Order!"):errs))










defOrganizePass :: BzoFileModel CallAST -> Either [BzoErr] (BzoFileModel [Definition T.Text])
defOrganizePass (BzoFileModel mn fp dm (CA_Calls _ model) fi fl ia la) =
  case (foldl defOrganizer (DefState [] []) model) of
    (DefState model' []) -> Right (BzoFileModel mn fp dm (reverse model') fi fl ia la)
    (DefState _      er) -> Left er










wrappedDefOrganizePass :: [BzoFileModel CallAST] -> Either [BzoErr] [BzoFileModel [Definition T.Text]]
wrappedDefOrganizePass xs =
  let xs' = map defOrganizePass xs
      xsl = concat $ E.lefts  xs'
      xsr = E.rights xs'
  in case xsl of
      [] -> Right xsr
      er -> Left  $ reverse xsl
