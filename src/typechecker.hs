module BzoChecker where
import BzoSyntax
import BzoTypes
import BzoPreprocessor
import GHC.Exts
import Data.Either
import Data.Maybe
import Data.List hiding (map, foldl, foldr, insert)
import Data.Map.Strict hiding (map, foldl, foldr)
import HigherOrder
import Debug.Trace










data BzoLiteral
  = LtStr String
  | LtInt Integer
  | LtFlt Double
  | LtId  String
  | LtFn  String
  | LtTy  String
  | LtTV  String
  | LtVr  String
  | LtNm  BzoLiteral String
  | LtArr BzoLiteral BzoType
  | LtNms BzoLiteral String










data BzoType
  = TyCmpd [BzoType]
  | TyPoly [BzoType]
  | TyLit   BzoLiteral
  | TyFnDf  BzoType BzoType
  | TyFilt  BzoType BzoType
  | TyExpr  BzoType BzoType
  | TyWild
  | TyNil










data BzoFileTypeData
  = BzoFileTypeData {
      bft_module    :: String,
      bft_domain    :: String,
      bft_ast       :: BzoSyntax,
      bft_functions :: [(String, [BzoSyntax])],
      bft_typedefs  :: [(String, [BzoSyntax])],
      bft_fntypes   :: [(String, [BzoSyntax])],
      bft_impfuncs  :: [String],
      bft_imptypes  :: [String],
      bft_impftypes :: [String] }










getImportDependencies :: BzoFileData -> [String]
getImportDependencies fs = nub $ (\(BzoFileData mn fp dm ast im ln ia la) -> im ++ (map fst ia)) fs










getLinkDependencies :: [BzoFileData] -> [String]
getLinkDependencies fs = nub $ concatMap (\(BzoFileData mn fp dm ast im ln ia la) -> ln ++ (map fst la)) fs










orderByImports :: Map String BzoFileData -> [BzoFileData] -> [BzoFileData] -> Either [BzoErr] [BzoFileData]
orderByImports mp out [] = Right out
orderByImports mp out fs =
  let (remain, next) = break (\x -> containsManyMembers mp $ getImportDependencies x) fs
      next'          = map (\x -> (bfd_moduleName x, x)) next
      domain         = if (bfd_domain (head fs) == "@")
                        then "Project Files"
                        else bfd_domain $ head fs
  in case next of
    [] -> Left [CfgErr ("Unsatisfiable Dependencies in " ++ domain ++ "! Compilation cannot continue.\n")]
    nx -> orderByImports (insertMany mp next') (out ++ next) remain










orderByLinks :: Map String [BzoFileData] -> [[BzoFileData]] -> [[BzoFileData]] -> Either [BzoErr] [[BzoFileData]]
orderByLinks mp out [] = Right out
orderByLinks mp out fs =
  let (remain, next) = break (\x -> containsManyMembers mp $ getLinkDependencies x) fs
      next'          = map (\x -> (bfd_domain $ head x, x)) next
  in case next of
    [] -> Left [CfgErr "Unsatisfiable Dependencies between libraries! Compilation cannot continue.\n"]
    nx -> orderByLinks (insertMany mp next') (out ++ next) remain









orderFileData :: [BzoFileData] -> Either [BzoErr] [BzoFileData]
orderFileData fs =
  let f0 = groupWith bfd_domain $ map appendStdDep fs
      f1 = map (orderByImports empty []) f0
      f2 = concat $ lefts  f1
      f3 = [orderByLinks empty [] $ rights f1]
      f4 = lefts  f3
      f5 = concat $ rights f3
  in case (f2, f4) of
      ([], []) -> Right $ concat f5
      ([], er) -> Left  $ concat er
      (er, _ ) -> Left  er










appendStdDep :: BzoFileData -> BzoFileData
appendStdDep (BzoFileData mn fp "Std" ast imp lnk ima lna) = (BzoFileData mn fp "Std" ast imp lnk ima lna)
appendStdDep (BzoFileData mn fp dmn   ast imp lnk ima lna) =
  case (elem "Std" imp, elem "Std" $ map fst ima) of
    (False, False) -> (BzoFileData mn fp dmn ast imp (lnk ++ ["Std"]) ima lna)
    (_    , _    ) -> (BzoFileData mn fp dmn ast imp lnk ima lna)










-- | Returns (Ids/BI Ids, TyIds/BI TyIds, MIds)
getIds :: BzoSyntax -> ([String], [String], [String])
getIds (BzS_Expr       p xs) = zip3Map $ map getIds xs
getIds (BzS_Block      p xs) = zip3Map $ map getIds xs
getIds (BzS_MId        p st) = ([  ], [  ], [st])
getIds (BzS_TyId       p st) = ([  ], [st], [  ])
getIds (BzS_Id         p st) = ([st], [  ], [  ])
getIds (BzS_BId        p st) = ([st], [  ], [  ])
getIds (BzS_BTId       p st) = ([  ], [st], [  ])
getIds (BzS_Filter     p xs) = getIds xs
getIds (BzS_Box        p xs) = getIds xs
getIds (BzS_ArrExprMod p xs) = getIds xs
getIds (BzS_Cmpd       p xs) = zip3Map $ map getIds xs
getIds (BzS_Poly       p xs) = zip3Map $ map getIds xs
getIds (BzS_FnTy    p as bs) = zip3Map [getIds as, getIds bs]
getIds (BzS_Calls      p xs) = zip3Map $ map getIds xs

getIds (BzS_FunDef     p i f o d) = zip3Map [getIds i, getIds o, getIds d, ([f], [ ], [ ])]
getIds (BzS_FnTypeDef  p   f   d) = zip3Map [                    getIds d, ([f], [ ], [ ])]
getIds (BzS_TypDef     p i t   d) = zip3Map [getIds i,           getIds d, ([ ], [t], [ ])]

getIds _                     = ([  ], [  ], [  ])










extractOutVarIds :: BzoSyntax -> Maybe [String]
extractOutVarIds (BzS_Id     p           st ) = Just [st]
extractOutVarIds (BzS_Box    p (BzS_Id o st)) = Just [st]
extractOutVarIds (BzS_Cmpd   p           xs ) =
  case (concat $ catMaybes $ map extractOutVarIds xs) of
    [] -> Nothing
    ms -> Just ms
extractOutVarIds (BzS_Expr   p           xs ) = fn Nothing (reverse xs)
  where fn :: Maybe [String] -> [BzoSyntax] -> Maybe [String]
        fn (Just xs)                       _  = Just xs
        fn Nothing  ((BzS_Filter    p fl):xs) = fn Nothing xs
        fn Nothing  ((BzS_Namespace p nm):xs) = fn Nothing xs
        fn Nothing  ((BzS_Id        p x ):xs) = Just [x]
        fn Nothing  ((BzS_BId       p x ):xs) = Just [ ]
        fn Nothing  ((BzS_BTId      p x ):xs) = Just [ ]
        fn Nothing  ((BzS_Wildcard  p   ):xs) = Just [ ]
        fn Nothing                         _  = Nothing










-- | Return (Function Names, Type Names, Fn Types)
getCallIds :: BzoSyntax -> ([(String, [BzoSyntax])], [(String, [BzoSyntax])], [(String, [BzoSyntax])])
getCallIds (BzS_Calls     p      xs) = zip3Map $ map getCallIds xs
getCallIds (BzS_FnTypeDef p   f   d) = ([                              ], [                           ], [(f, [BzS_FnTypeDef p   f   d])])
getCallIds (BzS_TypDef    p i t   d) = ([                              ], [(t, [BzS_TypDef p i t   d])], [                              ])
getCallIds (BzS_FunDef    p i f o d) = ([(f, [BzS_FunDef    p i f o d])], [                           ], [                              ])
getCallIds _                         = ([                              ], [                           ], [                              ])










getTypeData0 :: BzoSyntax -> BzoFileTypeData
getTypeData0 ast =
  let (fns , tys , fts ) = getCallIds ast
      (fns', tys', fts') = (shrinkPairs fns, shrinkPairs tys, shrinkPairs fts)
  in (BzoFileTypeData "" "" ast fns' tys' fts' [] [] [])










-- Adjust to return errors
modelType :: BzoSyntax -> BzoType
modelType (BzS_Cmpd ps exprs) = TyCmpd $ map modelType exprs
modelType (BzS_Poly ps exprs) = TyPoly $ map modelType exprs
modelType (BzS_Wildcard ps  ) = TyWild
modelType (BzS_Nil      ps  ) = TyNil
modelType (BzS_Str  ps  str ) = LtStr str
modelType (BzS_Int  ps  num ) = LtInt num
modelType (BzS_Flt  ps  num ) = LtFlt num
modelType (BzS_Id   ps  str ) = LtId  str   -- Later, check if id is a global function
modelType (BzS_BId  ps  str ) = LtId  str   -- Later, check if id is a valid builtin
modelType (BzS_TyId ps  str ) = LtTy  str   -- Later, make Type or TyVar depending on if str is globally defined
modelType (BzS_BTId ps  str ) = LtTy  str   -- Later, check if id is a valid builtin
--modelType (BzS_MId  ps  str ) = ERR
--modelType (BzS_Expr ps atoms) =










{-
generateSymbolTables :: [BzoFileData] -> Either [BzoErr] [BzoFileTypeData]
generateSymbolTables fd =
  let t0 = map (getTypeData0 . bfd_fileAST) fd
      t1 = map bfd_moduleName fd
      t2 = map bfd_domain     fd
      t3 = map (\(mn, dm, (BzoFileTypeData _ _ ast fns tys fts _ _ _)) -> (BzoFileTypeData mn dm ast fns tys fts [] [] []))  $ zip3 t0 t1 t2
      t4 = insertMany empty $ shrinkPairs $ map (\(BzoFileTypeData mn dm _ fns tys fts _ _ _) -> (dm, )) fd
-}
