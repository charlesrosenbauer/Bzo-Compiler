module IRModelAbstract where
import Data.Text
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Either
import Data.Tuple
import IRParser
import IRTypes
import HigherOrder
import Debug.Trace










makeIntList :: [IRParseItem] -> Either [IRErr] [Int]
makeIntList []                  = Right []
makeIntList ((PI_Int ips ix):irs) =
  case (makeIntList irs) of
    Left ers -> Left ers
    Right xs -> Right (ix:xs)

makeIntList (ir:irs) =
  case (makeIntList irs) of
    Left ers -> Left ((IRErr (ppos ir) $ pack "Invalid operand.\n"):ers)
    Right xs -> Left [IRErr (ppos ir) $ pack "Invalid operand.\n"]










getFnid :: IRSymbols -> IRParseItem -> Either IRErr FnId
getFnid syms (PI_Func fps fid) =
  case (M.lookup fid $ funcSymbols syms) of
    Just fx -> Right fx
    Nothing -> Left  $ IRErr fps $ append fid $ pack " is an undefined function.\n"

getFnid syms (PI_Proc fps fid) =
  case (M.lookup fid $ funcSymbols syms) of
    Just fx -> Right fx
    Nothing -> Left  $ IRErr fps $ append fid $ pack " is an undefined procedure.\n"

getFnid syms (PI_Extn fps fid) =
  case (M.lookup fid $ funcSymbols syms) of
    Just fx -> Right fx
    Nothing -> Left  $ IRErr fps $ append fid $ pack " is an undefined external function.\n"










-- | Assumes [IRParseItem] is [PI_Type]
undefTypes :: String -> IRSymbols -> [IRParseItem] -> ([IRErr], [TyId])
undefTypes ermsg syms []      = ([], [])
undefTypes ermsg syms (x@(PI_Type p _ t):irs) =
  let (ers, ids) = undefTypes ermsg syms irs
      tyid       = M.lookup t $ typeSymbols syms
  in case tyid of
      Nothing -> ((IRErr p $ pack $ (unpack $ t) ++ ermsg):ers, ids)
      Just ty -> (ers, ty:ids)

undefTypes ermsg syms (_:irs) = undefTypes ermsg syms irs










makeTypeRef :: IRSymbols -> IRParseItem -> Either IRErr TypeRef
makeTypeRef syms (PI_Type tps tns tid) =
  case (M.lookup tid $ typeSymbols syms) of
    Just tx -> Right $ TypeRef tns tx
    Nothing -> Left  $ IRErr   tps $ append tid $ pack " is an undefined type.\n"










getConstId :: IRSymbols -> IRParseItem -> Either IRErr CnstId
getConstId syms (PI_Const cps cid) =
  case (M.lookup cid $ cnstSymbols syms) of
    Just cx -> Right $ cx
    Nothing -> Left  $ IRErr   cps $ append cid $ pack " is an undefined constant.\n"
