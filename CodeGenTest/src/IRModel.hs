module IRModel where
import Data.Text
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Either
import Data.Tuple
import IRParser
import IRLexer
import IRTypes
import IRFuncModel
import IRTypeModel
import IRModelAbstract
import HigherOrder
import Debug.Trace










modelIR :: IRParseItem -> Either [IRErr] (IRSymbols,
                                          Map FnId FuncData,
                                          Map TyId TypeData,
                                          Map CnstId ConstantData,
                                          [Hint])
modelIR irs =
  let (errs, syms) = getSymbols   [irs]
      fns          = getFunctions [irs]
      tys          = getTypes     [irs]
      cts          = getConstants [irs]
      cts'         = modelConst    cts syms
      (fers, fns') = modelFunc     fns syms
      (ters, tys') = modelType     tys syms
  in case (fers ++ errs) of
      [] -> Right (syms, (makeFuncMap fns'), (makeTypeMap tys'), M.empty, [])
      er -> Left er










makeFuncMap :: [FuncData] -> Map FnId FuncData
makeFuncMap fs = M.fromList $ L.map (\f@(FuncType _ _ _ _ _ _ _ fnid) -> (fnid, f)) fs










makeTypeMap :: [TypeData] -> Map TyId TypeData
makeTypeMap ts = M.fromList $ L.map (\t@(TypeData _ _ _ _ tyid) -> (tyid, t)) ts









getFunctions :: [IRParseItem] -> [IRParseItem]
getFunctions [] = []
getFunctions ((PI_Defs _ defs)          :irs) = getFunctions (irs ++ defs)
getFunctions (fn@(PI_FnDef _ _ _ _    ) :irs) = fn : (getFunctions irs)
getFunctions (fn@(PI_PrDef _ _ _ _ _ _) :irs) = fn : (getFunctions irs)
getFunctions (fn@(PI_ExDef _ _ _ _ _ _) :irs) = fn : (getFunctions irs)
getFunctions (x                         :irs) =      (getFunctions irs)










getTypes :: [IRParseItem] -> [IRParseItem]
getTypes [] = []
getTypes ((PI_Defs _ defs)      :irs) = getTypes (irs ++ defs)
getTypes (fn@(PI_TyDef _ _ _ _) :irs) = fn : (getTypes irs)
getTypes (x                     :irs) =      (getTypes irs)










getConstants :: [IRParseItem] -> [IRParseItem]
getConstants [] = []
getConstants ((PI_Defs _ defs)       :irs) = getTypes (irs ++ defs)
getConstants (fn@(PI_Const    _ _  ) :irs) = fn : (getTypes irs)
getConstants (fn@(PI_ConstInt _ _ _) :irs) = fn : (getTypes irs)
getConstants (fn@(PI_ConstStr _ _ _) :irs) = fn : (getTypes irs)
getConstants (x                      :irs) =      (getTypes irs)










modelConst :: [IRParseItem] -> IRSymbols -> [ConstantData]
modelConst [] syms = []
modelConst ((PI_Const    p x  ):irs) syms = (ConstBool ((cnstSymbols syms) ! x)  ) : (modelConst irs syms)
modelConst ((PI_ConstStr p x s):irs) syms = (ConstStr  ((cnstSymbols syms) ! x) s) : (modelConst irs syms)
modelConst ((PI_ConstInt p x n):irs) syms = (ConstInt  ((cnstSymbols syms) ! x) n) : (modelConst irs syms)
modelConst (_                  :irs) syms = modelConst irs syms










getSymbols :: [IRParseItem] -> ([IRErr], IRSymbols)
getSymbols [] = ([], initialSymbolTable)

getSymbols ((PI_Defs _ defs):irs) = getSymbols (irs ++ defs)

getSymbols ((PI_FnDef p fnid _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' cns cns' top'))

getSymbols ((PI_TyDef p tyid _ _):irs) =
  let (errs, (IRSymbols fns fns'  tys  tys' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup tyid tys) top (top+1)
      ntys' = M.insert top' tyid tys'
      ntys  = M.insert tyid top' tys
      errs' = ife (top /= top') errs ((IRErr p $ append tyid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' ntys ntys' cns cns' (top+1)))

getSymbols ((PI_PrDef p fnid _ _ _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' cns cns' (top+1)))

getSymbols ((PI_ExDef p fnid _ _ _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' cns cns' (top+1)))

getSymbols ((PI_Const p cid):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ncns ncns' (top+1)))

getSymbols ((PI_ConstInt p cid _):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ncns ncns' (top+1)))

getSymbols ((PI_ConstStr p cid _):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ncns ncns' (top+1)))

getSymbols (_:irs) = getSymbols irs
