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










defOrganizer :: DefState -> CallAST -> DefState
defOrganizer (DefState ds errs) t@(CA_TypeDefCall p i prs _ _ d) =
  (DefState (TyDefinition (prs, d, t, p) (T.pack i):ds) errs)

defOrganizer (DefState ds errs) t = (DefState ds ((SntxErr (ca_pos t) "Unexpected Definition Order!"):errs))










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
      er -> Left  xsl
