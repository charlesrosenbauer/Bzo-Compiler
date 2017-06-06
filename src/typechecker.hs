module BzoChecker where
import BzoSyntax
import BzoTypes
import BzoPreprocessor
import GHC.Exts
import Data.Either
import Data.Maybe
import Data.List hiding (map, foldl, foldr, insert)
import Data.Map.Strict hiding (map, foldl, foldr, mapEither)
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










isValidBId :: String -> Bool
isValidBId "$add-binop"    = True
isValidBId "$sub-binop"    = True
isValidBId "$mul-binop"    = True
isValidBId "$div-binop"    = True
isValidBId "$mod-binop"    = True
isValidBId "$gtr-binop"    = True
isValidBId "$lss-binop"    = True
isValidBId "$geq-binop"    = True
isValidBId "$leq-binop"    = True
isValidBId "$eql-binop"    = True
isValidBId "$neq-binop"    = True
isValidBId "$arrLength"    = True
isValidBId "$getIndex"     = True
isValidBId "$setIndex"     = True
isValidBId "$map"          = True
isValidBId "$fold"         = True
isValidBId "$reduce"       = True
isValidBId "$scam"         = True
isValidBId "$chain"        = True
isValidBId "$zip"          = True
isValidBId "$exp-binop"    = True
isValidBId "$surd-binop"   = True
isValidBId "$log-binop"    = True
isValidBId "$sine"         = True
isValidBId "$cosine"       = True
isValidBId "$tangent"      = True
isValidBId "$arcsine"      = True
isValidBId "$arccosine"    = True
isValidBId "$arctangent"   = True
isValidBId "$hsine"        = True
isValidBId "$hcosine"      = True
isValidBId "$htangent"     = True
isValidBId "$arc-hsine"    = True
isValidBId "$arc-hcosine"  = True
isValidBId "$arc-htangent" = True
isValidBId "$floor"        = True
isValidBId "$ciel"         = True
isValidBId "$round"        = True
isValidBId "$factorial"    = True
isValidBId "$gamma"        = True
isValidBId "$nCr"          = True
isValidBId "$nPr"          = True
isValidBId "$sqrt"         = True
isValidBId "$cbrt"         = True
isValidBId "$log2"         = True
isValidBId "$log10"        = True
isValidBId "$to-uppercase" = True
isValidBId "$to-lowercase" = True
isValidBId "$isPrefix"     = True
isValidBId "$isSuffix"     = True
isValidBId "$append-array" = True
isValidBId "$print"        = True
isValidBId "and-binop"     = True
isValidBId "$or-binop"     = True
isValidBId "$xor-binop"    = True
isValidBId "$not-op"       = True
isValidBId "$rshift-binop" = True
isValidBId "$lshift-binop" = True
isValidBId "$ctlz-op"      = True
isValidBId "$cttz-op"      = True
isValidBId "$popcount-op"  = True
isValidBId "$toInt"        = True
isValidBId "$toUnt"        = True
isValidBId "$toUnm"        = True
isValidBId "$toFlt"        = True
isValidBId "$toBits"       = True
isValidBId _               = False










isValidBTId :: String -> Bool
isValidBTId "$Int8"      = True
isValidBTId "$Int16"     = True
isValidBTId "$Int32"     = True
isValidBTId "$Int64"     = True
isValidBTId "$Unt8"      = True
isValidBTId "$Unt16"     = True
isValidBTId "$Unt32"     = True
isValidBTId "$Unt64"     = True
isValidBTId "$Flt16"     = True
isValidBTId "$Flt32"     = True
isValidBTId "$Flt64"     = True
isValidBTId "$Unm16"     = True
isValidBTId "$Unm32"     = True
isValidBTId "$Unm64"     = True
isValidBTId "$BoolTF"    = True
isValidBTId "$BoolFT"    = True
isValidBTId "$Char"      = True
isValidBTId "$ASCII"     = True
isValidBTId "$Unique"    = True
isValidBTId "$Effect"    = True
isValidBTId "$Byte"      = True
isValidBTId "$Half"      = True
isValidBTId "$Word"      = True
isValidBTId "$Dword"     = True
isValidBTId _            = False










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










-- | Return (Function Names, Type Names, Fn Types)
getCallIds :: BzoSyntax -> ([(String, BzoSyntax)], [(String, BzoSyntax)], [(String, BzoSyntax)])
getCallIds (BzS_Calls     p      xs) = zip3Map $ map getCallIds xs
getCallIds (BzS_FnTypeDef p   f   d) = ([                            ], [                         ], [(f, BzS_FnTypeDef p   f   d)])
getCallIds (BzS_TypDef    p i t   d) = ([                            ], [(t, BzS_TypDef p i t   d)], [                            ])
getCallIds (BzS_FunDef    p i f o d) = ([(f, BzS_FunDef    p i f o d)], [                         ], [                            ])
getCallIds _                         = ([                            ], [                         ], [                            ])
