module BzoChecker where
import BzoTypes
import SymbolTable
import Data.Int
import qualified Data.Text       as T
import qualified Data.Either     as E
import qualified Data.Map.Strict as M
import qualified Data.Maybe      as Mb
import qualified Data.List       as L
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










getFIdSet :: SymbolTable -> [T.Text] -> [T.Text] -> [Int64]
getFIdSet (SymbolTable iids fids itab ftab dmid itop ftop) imps lnks =
  let lnks' =          Mb.catMaybes $ map (\x -> M.lookup x fids) lnks
      imps' = concat $ Mb.catMaybes $ map (\x -> M.lookup x dmid) imps
  in L.nub (lnks' ++ imps')










-- Hash values are temporary for now
constructType :: SymbolTable -> TypeAST -> Either [BzoErr] BzoType
constructType st (TA_Nil    _      ) = Right $ BT_Nil 0
constructType st (TA_IntLit _ i    ) = Right $ BT_Int 1 i
constructType st (TA_FltLit _ f    ) = Right $ BT_Flt 2 f
constructType st (TA_StrLit _ s    ) = Right $ BT_Str 3 (T.pack s)
constructType st (TA_Arr    _ sz t ) =
  case (constructType st t) of
    Left errs -> Left  errs
    Right typ -> Right (BT_Arr   4 typ (map fromInteger sz))

constructType st (TA_Cmpd   _    ts) =
  let types = map (constructType st) ts
  in case (E.lefts types) of
      []      -> Right $ BT_Cmpd 5 (E.rights types)
      ers     -> Left  $ concat ers

constructType st (TA_Poly   _    ts) =
  let types = map (constructType st) ts
  in case (E.lefts types) of
      []      -> Right $ BT_Poly 6 (E.rights types)
      ers     -> Left  $ concat ers

constructType st (TA_Expr   _ hd tl) =
  let headtype = (constructType st) hd
      tailtype = (constructType st) tl
  in case (headtype, tailtype) of
      (Right h, Right t) -> Right $ BT_Expr 7 h t
      (Left  h, Right t) -> Left h
      (Right h, Left  t) -> Left t
      (Left  h, Left  t) -> Left (h ++ t)

constructType st (TA_FnTy   _ it xt) =
  let intype = (constructType st) it
      extype = (constructType st) xt
  in case (intype, extype) of
      (Right i, Right x) -> Right $ BT_Expr 7 i x
      (Left  i, Right x) -> Left i
      (Right i, Left  x) -> Left x
      (Left  i, Left  x) -> Left (i ++ x)
