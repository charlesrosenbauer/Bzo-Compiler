module BzoChecker where
import BzoTypes
import SymbolTable
import Data.Int
import HigherOrder
import Builtins
import qualified Data.Text       as T
import qualified Data.Either     as E
import qualified Data.Map.Strict as M
import qualified Data.Maybe      as Mb
import qualified Data.List       as L
import qualified Data.Set        as S
import qualified Data.Tuple      as Tp
import Debug.Trace









{-
data DefState
  = DefState {
      ds_defs    :: [Definition T.Text],
      ds_errs    :: [BzoErr] }










defOrganizer :: DefState -> CallAST -> DefState
defOrganizer (DefState [] errs) t@(CA_TypeDefCall p i prs df) =
  (DefState ([TyDefinition (prs, df, p) (T.pack i)]) errs)

defOrganizer (DefState ds errs) t@(CA_TypeDefCall p i prs df) =
  (DefState ([TyDefinition (prs, df, p) (T.pack i)] ++ ds) errs)

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










-- | Provides a list of all file ids visible, given a set of imports and links
getFIdSet :: SymbolTable -> [T.Text] -> [T.Text] -> [Int64]
getFIdSet (SymbolTable iids fids itab ftab dmid itop ftop) imps lnks =
  let lnks' =          Mb.catMaybes $ map (\x -> M.lookup x fids) lnks
      imps' = concat $ Mb.catMaybes $ map (\x -> M.lookup x dmid) imps
  in L.nub (lnks' ++ imps')










-- | Provides a table of all Identifiers visible to a certain file
getNamespaces :: Show a => SymbolTable -> BzoFileModel a -> Either [BzoErr] NameTable
getNamespaces st (BzoFileModel mn _ dm _ imps lnks impas lnkas) =
  let domain'   = T.pack dm
      self      = T.pack mn
      imports'  = map T.pack imps
      links'    = map T.pack lnks
      impsAs'   = map (\(a, b) -> (T.pack a, T.pack b)) impas
      linkAs'   = map (\(a, b) -> (T.pack a, T.pack b)) lnkas

      linkfiles = map (\(a,b) -> (M.lookup a $ st_dmids st, b)) ((map (\x -> (x,x)) links'  ) ++ linkAs')
      impfiles  = map (\(a,b) -> (M.lookup a $ st_fids  st, b)) ((map (\x -> (x,x)) imports') ++ impsAs')
      allfiles  = linkfiles ++ (map (\(a,b) -> ((fmap (\x -> [x]) a), b)) impfiles)

      maybenames= map fst allfiles
      names     = Mb.catMaybes maybenames
      maybefails= map snd $ filter (Mb.isNothing. fst) allfiles
      errs0     = ife (length maybenames /= length names) [(DepErr ("In module " ++ mn ++ ", the following links/imports failed: " ++ (concatMap (\x -> show x ++ ", ")  maybefails)))] []

      names'    = L.nub names
      errs1     = ife (length names /= length names') [(DepErr ("In module " ++ mn ++ ", the following are duplicate namespaces: " ++ (show $ names L.\\ names')))] []

      namePairs    = map (\(a,b) -> (b, Mb.fromJust a)) allfiles
      namePairs'   = concatMap (\(a,bs) -> zip (repeat a) bs) namePairs
      namePairSwap = map Tp.swap namePairs'

      errs      = errs0 ++ errs1
  in case errs of
      [] -> Right (NameTable M.empty M.empty (M.fromList namePairs) (M.fromList namePairSwap))
      er -> Left  er










getTypeVarsHelper :: TypeAST -> S.Set T.Text
getTypeVarsHelper (TA_Cmpd   _ xs   ) = S.unions $ map getTypeVarsHelper xs
getTypeVarsHelper (TA_Poly   _ xs   ) = S.unions $ map getTypeVarsHelper xs
getTypeVarsHelper (TA_Filt   _ ft _ ) = S.unions $ map getTypeVarsHelper ft
getTypeVarsHelper (TA_FnTy   _ it xt) = S.union (getTypeVarsHelper it) (getTypeVarsHelper xt)
getTypeVarsHelper (TA_Enum   _ _ _ x) = getTypeVarsHelper x
getTypeVarsHelper (TA_Record _ _ _ x) = getTypeVarsHelper x
getTypeVarsHelper (TA_Curry  _ cy x ) = S.unions $ map getTypeVarsHelper (x:cy)
getTypeVarsHelper (TA_Arr    _ _  x ) = getTypeVarsHelper x
getTypeVarsHelper (TA_TyVar  _ i    ) = S.singleton $ T.pack i
getTypeVarsHelper _                   = S.empty

-- | Takes a TypeAST and returns map to/from Identifiers/Local Identifiers
getTypeVars :: TypeAST -> (M.Map Int64 T.Text, M.Map T.Text Int64)
getTypeVars t =
  let pairs = zip [1..] $ S.elems $ getTypeVarsHelper t
      flips = map Tp.swap pairs
  in (M.fromList pairs, M.fromList flips)










-- Returns a set of all identifiers visible in a given namespace

type VisibleIds = S.Set T.Text
{-
getVisibleIds :: SymbolTable -> NameTable -> VisibleIds
getVisibleIds st nt =
  let allIds = concatMap (\(a, bs) -> zip (repeat a) $ map snd bs) $ M.assocs $ st_iids st
      spaces = S.fromList $ L.nub $ concat $ M.elems nt
  in S.fromList $ map fst $ filter (\(a, b) -> S.member b spaces) allIds -}










constructType :: Int64 -> (VisibleIds, NameTable) -> M.Map T.Text Int64 -> SymbolTable -> TypeAST -> Either [BzoErr] BzoType
constructType fid nt vt st (TA_Nil    _      ) = Right $ BT_Nil (hashInt 0)
constructType fid nt vt st (TA_IntLit _ i    ) = Right $ BT_Int (hash i) i
constructType fid nt vt st (TA_FltLit _ f    ) = Right $ BT_Flt (hash f) f
constructType fid nt vt st (TA_StrLit _ s    ) = Right $ BT_Str (hash s) (T.pack s)
constructType fid nt vt st (TA_Arr    _ sz t ) =
  let sizes = map fromInteger sz
      sizeHash = hash sizes
  in case (constructType fid nt vt st t) of
    Left errs -> Left  errs
    Right typ -> Right (BT_Arr   (hash [bt_hash typ, sizeHash]) typ sizes)

constructType fid nt vt st (TA_Cmpd   _    ts) =
  let types  = map (constructType fid nt vt st) ts
      hashes = hash $ E.rights types
  in case (E.lefts types) of
      []      -> Right $ BT_Cmpd hashes (E.rights types)
      ers     -> Left  $ concat ers

constructType fid nt vt st (TA_Poly   _    ts) =
  let types  = map (constructType fid nt vt st) ts
      hashes = hash $ E.rights types
  in case (E.lefts types) of
      []      -> Right $ BT_Poly hashes (E.rights types)
      ers     -> Left  $ concat ers

constructType fid nt vt st (TA_Expr   _ hd tl) =
  let headtype = (constructType fid nt vt st) hd
      tailtype = (constructType fid nt vt st) tl
  in case (headtype, tailtype) of
      (Right h, Right t) -> Right $ BT_Expr (hash [h, t]) h t
      (Left  h, Right t) -> Left h
      (Right h, Left  t) -> Left t
      (Left  h, Left  t) -> Left (h ++ t)

constructType fid nt vt st (TA_FnTy   _ it xt) =
  let intype = (constructType fid nt vt st) it
      extype = (constructType fid nt vt st) xt
  in case (intype, extype) of
      (Right i, Right x) -> Right $ BT_Expr (hash [i, x]) i x
      (Left  i, Right x) -> Left i
      (Right i, Left  x) -> Left x
      (Left  i, Left  x) -> Left (i ++ x)

constructType fid nt vt st (TA_TyVar  _ vr) =
  let x = vt M.! (T.pack vr)
  in Right $ BT_TVar (hashInt x) x

constructType fid nt vt st (TA_BFnLit p f) =
  case (isBuiltinFunc $ T.pack f) of
    0 -> Left  [ TypeErr p (f ++ " is not a valid builtin function.") ]
    x -> Right $ BT_BFun (hashInt x) x

constructType fid nt vt st (TA_BTyLit p t) =
  case (isBuiltinType $ T.pack t) of
    0 -> Left  [ TypeErr p (t ++ " is not a valid builtin type.") ]
    x -> Right $ BT_BTyp (hashInt x) x

constructType fid (vi, nt) vt st (TA_FnLit p f) =
  let xid  = L.lookup fid $ Mb.fromJust $ M.lookup (T.pack f) (st_iids st)
      xid' = Mb.fromJust xid
  in if ((S.member (T.pack f) vi) && (Mb.isJust xid))
      then Right $ BT_Func (hashInt xid') xid' fid
      else Left  [ TypeErr p ("Function " ++ f ++ " is undefined.")]

constructType fid (vi, nt) vt st (TA_TyLit p t) =
  let xid  = L.lookup fid $ Mb.fromJust $ M.lookup (T.pack t) (st_iids st)
      xid' = Mb.fromJust xid
  in if ((S.member (T.pack t) vi) && (Mb.isJust xid))
      then Right $ BT_Type (hashInt xid') xid' fid
      else Left  [ TypeErr p ("Type " ++ t ++ " is undefined.")]

constructType fid (vi, nt) vt st _ = Right $ BT_Nil 0 -- Temporary









{-
generateTypesHelper :: Int64 -> VisibleIds -> NameTable -> SymbolTable -> [Definition T.Text] -> ([BzoErr], [BzoType])
generateTypesHelper fid vn nt st [] = ([], [])
generateTypesHelper fid vn nt st ((TyDefinition (_, tast, _, _, _) _):xs) =
  let (_, tvars)   = getTypeVars tast
      bzotype      = constructType fid (vn, nt) tvars st tast
      (errs, typs) = generateTypesHelper fid vn nt st xs
  in case bzotype of
      Left  err -> (err++errs, typs)
      Right typ -> (errs,  typ:typs)
generateTypesHelper fid vn nt st (def:xs) = generateTypesHelper fid vn nt st xs









generateTypes :: SymbolTable -> [BzoFileModel [Definition T.Text]] -> ([BzoErr], [BzoType])
generateTypes st [] = ([], [])
generateTypes st (fm@(BzoFileModel mn _ dm model _ _ _ _) : fms) =
  let nametable      = getNamespaces st fm
      nametable'     = head $ E.rights [nametable]
      fid            = Mb.fromJust $ M.lookup (T.pack $ mn ++ ":" ++ dm) (st_fids st)
      visibleids     = getVisibleIds st nametable'
      (errs , typs ) = generateTypes st fms
      (errs', typs') = generateTypesHelper fid visibleids nametable' st model
  in case nametable of
      Left  err -> (err ++ errs, typs)
      Right _   -> (errs' ++ errs, typs' ++ typs)









-- This function and it's helpers are mostly just testing functions for now.
wrappedGenerateTypes :: Either [BzoErr] SymbolTable -> Either [BzoErr] [BzoFileModel [Definition T.Text]] -> ([BzoErr], [BzoType])
wrappedGenerateTypes (Left errs0) (Left errs1) = (errs0 ++ errs1, [])
wrappedGenerateTypes (Left  errs) (Right _   ) = (errs, [])
wrappedGenerateTypes (Right _   ) (Left  errs) = (errs, [])
wrappedGenerateTypes (Right st  ) (Right fms) = trace (show st) $ generateTypes st fms-}
-}
