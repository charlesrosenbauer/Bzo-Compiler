module IRFuncModel where
import Data.Text
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Either
import Data.Tuple
import IRParser
import IRTypes
import IRModelAbstract
import HigherOrder
import Debug.Trace










modelFunc :: [IRParseItem] -> IRSymbols -> ([IRErr], [FuncData])
modelFunc [] syms = ([], [])
modelFunc ((PI_FnDef  p fnid pars def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters
      -- Model Contents
      (er0, nds) = modelFuncNodes p syms def

      -- Model the rest of the list
      (ers, fns) = modelFunc irs syms
  in (er0 ++ ers, L.map buildFuncNodeModel $ ((FuncType PureFn [] [] [] [] [] nds fnid' M.empty M.empty 0):fns))

modelFunc ((PI_PrDef  p fnid pars refx wefx def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters

      -- Model Effect Types
      ermsg = " is an undefined type, and cannot be used as an effect.\n"
      (er1, rfx) = undefTypes ermsg syms refx
      (er2, wfx) = undefTypes ermsg syms wefx

      -- Model Contents
      (er0, nds) = modelFuncNodes p syms def

      -- Model the rest of the list
      (ers, fns) = modelFunc irs syms
  in (ers ++ er1 ++ er2, L.map buildFuncNodeModel $ (FuncType ProcFn [] [] rfx wfx [] nds fnid' M.empty M.empty 0):fns)

modelFunc ((PI_ExDef  p fnid pars refx wefx def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters

      -- Model Effect Types
      ermsg = " is an undefined type, and cannot be used as an effect.\n"
      (er1, rfx) = undefTypes ermsg syms refx
      (er2, wfx) = undefTypes ermsg syms wefx

      -- Model Contents
      (er0, nds) = modelFuncNodes p syms def

      -- Model the rest of the list
      (ers, fns) = modelFunc irs syms
  in (ers ++ er1 ++ er2, L.map buildFuncNodeModel $ (FuncType ExtrFn [] [] rfx wfx [] nds fnid' M.empty M.empty 0):fns)











modelFuncNodes :: IRPos -> IRSymbols -> [IRParseItem] -> ([IRErr], [Node])
modelFuncNodes ps syms irs =
  let (ers, nds) = L.foldl (modelFuncNode syms) ([], []) irs

      -- TODO: Verify that all the nodes fit together nicely
      (outss, inss) = L.unzip $ L.map (\nd -> (nodeOuts nd, getIns nd)) nds
      outs = L.concat outss
      outs'= L.nub outs
      er0  = if outs /= outs'
              then [(IRErr ps $ pack $ "Function contains repeated SSA variables: " ++ (show $ L.map (\x -> "#" ++ (show x)) (outs L.\\ outs')) ++ "\n")]
              else []

      iopairs = L.zip inss outss
      er1  = catMaybes $ L.map (\(i, o) -> checkIns ps (o, i, outs)) iopairs
  in (er0++er1++ers, nds)
  where checkIns :: IRPos -> ([Int], [Int], [Int]) -> Maybe IRErr
        checkIns ps ([],  _,  _) = Nothing
        checkIns ps (os, is, xs) =
          let osmin   = L.minimum os
              isfail0 = L.filter (\x -> x >= osmin) is
              isfail1 = L.filter (\x -> L.notElem x xs) is
          in case (isfail0, isfail1) of
              ([], []) -> Nothing
              -- These error messages suck, but IR code isn't meant to be a serious language, so who cares?
              (f0, []) -> Just $ IRErr ps $ pack ("Cyclical dependencies in function definition, involving variables " ++ (show f0) ++ "\n")
              ([], f1) -> Just $ IRErr ps $ pack ("Calls to undefined variables in function definition: " ++ (show f1) ++ "\n")
              (f0, f1) -> Just $ IRErr ps $ pack ("Cyclical dependencies, and calls to undefined variables in function definition: " ++ (show f1) ++ "\n")










modelFuncNode :: IRSymbols -> ([IRErr], [Node]) -> IRParseItem -> ([IRErr], [Node])
modelFuncNode syms state (PI_Node p ns op pars) =
  case (ns, unpack op, L.reverse pars) of
    ([n], "input" , [t@(PI_Type tps tns tid)])                  -> appendEither (onRight       (ParNode  n)      (makeTypeRef syms t)) state
    ([n], "output", [t@(PI_Type tps tns tid), (PI_Int ips ix)]) -> appendEither (onRight (\x -> RetNode  n x ix) (makeTypeRef syms t)) state
    ([n], "cast"  , [t@(PI_Type tps tns tid), (PI_Int ips ix)]) -> appendEither (onRight (\x -> CastNode n x ix) (makeTypeRef syms t)) state

    ([n], "load"  , [t@(PI_Type tps tns tid), (PI_Int ips ix)                 ]) -> appendEither (onRight (\x -> LoadNode  n x ix   ) (makeTypeRef syms t)) state
    ([n], "stor"  , [t@(PI_Type tps tns tid), (PI_Int ips ix), (PI_Int jps jx)]) -> appendEither (onRight (\x -> StoreNode n x ix jx) (makeTypeRef syms t)) state

    -- These need to be tweaked a bit; I need some way to properly represent value indices that are independent of local variables
    ([n], "get"   , [t@(PI_Type tps tns tid), (PI_Int ips ix), (PI_Int jps jx)                 ]) -> appendEither (onRight (\x -> GetNode n x ix jx   ) (makeTypeRef syms t)) state
    ([n], "set"   , [t@(PI_Type tps tns tid), (PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (onRight (\x -> SetNode n x ix jx kx) (makeTypeRef syms t)) state

    ([n], "ifma"  , [(PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (Right (TrinopNode n UndefType IFMAOp  ix jx kx)) state
    ([n], "ifms"  , [(PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (Right (TrinopNode n UndefType IFMSOp  ix jx kx)) state
    ([n], "ffma"  , [(PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (Right (TrinopNode n UndefType FFMAOp  ix jx kx)) state
    ([n], "ffms"  , [(PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (Right (TrinopNode n UndefType FFMSOp  ix jx kx)) state

    ([n], "iadd"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IAddOp  ix jx)) state
    ([n], "isub"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType ISubOp  ix jx)) state
    ([n], "imul"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IMulOp  ix jx)) state
    ([n], "idiv"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IDivOp  ix jx)) state
    ([n], "imod"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IModOp  ix jx)) state
    ([n], "imax"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IMaxOp  ix jx)) state
    ([n], "imin"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IMinOp  ix jx)) state
    ([n], "fadd"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FAddOp  ix jx)) state
    ([n], "fsub"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FSubOp  ix jx)) state
    ([n], "fmul"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FMulOp  ix jx)) state
    ([n], "fdiv"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FDivOp  ix jx)) state
    ([n], "fmod"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FModOp  ix jx)) state
    ([n], "fmax"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FMaxOp  ix jx)) state
    ([n], "fmin"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FMinOp  ix jx)) state

    ([n], "or"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType OrOp    ix jx)) state
    ([n], "xor"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType AndOp   ix jx)) state
    ([n], "and"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType XorOp   ix jx)) state
    ([n], "lor"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LOrOp   ix jx)) state
    ([n], "land"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LAndOp  ix jx)) state
    ([n], "lxor"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LXorOp  ix jx)) state
    ([n], "ls"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n LSCond  ix jx)) state
    ([n], "gt"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n GTCond  ix jx)) state
    ([n], "leq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n LECond  ix jx)) state
    ([n], "geq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n GECond  ix jx)) state
    ([n], "eq"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n EQCond  ix jx)) state
    ([n], "neq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n NECond  ix jx)) state
    ([n], "ez"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n EZCond  ix jx)) state
    ([n], "nz"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n NZCond  ix jx)) state
    ([n], "lshl"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LShLOp  ix jx)) state
    ([n], "lshr"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LShROp  ix jx)) state
    ([n], "ashr"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType AShROp  ix jx)) state
    ([n], "rotr"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType RRotOp  ix jx)) state
    ([n], "rotl"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LRotOp  ix jx)) state
    ([n], "bdep"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType BExtOp  ix jx)) state
    ([n], "bext"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType BDepOp  ix jx)) state
    ([n], "lsbfill",[(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType LSBFillOp ix   )) state
    ([n], "lcbfill",[(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType LCBFillOp ix   )) state
    ([n], "cttz"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CttzOp  ix   )) state
    ([n], "ctlz"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CtlzOp  ix   )) state
    ([n], "pcnt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType PCntOp  ix   )) state
    ([n], "brev"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType BRevsOp ix   )) state
    ([n], "bswap" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType BSwapOp ix   )) state

    ([n], "logf"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FLogOp  ix jx)) state
    ([n], "rtf"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FRootOp ix jx)) state
    ([n], "expf"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FExpOp  ix jx)) state
    ([n], "logn"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType NLogOp  ix jx)) state
    ([n], "rtn"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType NRootOp ix jx)) state
    ([n], "expn"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType NExpOp  ix jx)) state
    ([n], "pow"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType PowOp   ix jx)) state
    ([n], "abs"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AbsOp   ix   )) state

    ([n], "trnc"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType TrncOp  ix   )) state
    ([n], "wide"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType WideOp  ix   )) state
    ([n], "neg"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType NegOp   ix   )) state
    ([n], "not"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType NotOp   ix   )) state
    ([n], "lnot"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType LNotOp  ix   )) state
    ([n], "sqrt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType SqrtOp  ix   )) state
    ([n], "cbrt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CbrtOp  ix   )) state
    ([n], "lg2"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType Lg2Op   ix   )) state
    ([n], "lg10"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType Lg10Op  ix   )) state
    ([n], "sin"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType SinOp   ix   )) state
    ([n], "cos"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CosOp   ix   )) state
    ([n], "tan"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType TanOp   ix   )) state
    ([n], "asin"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AsinOp  ix   )) state
    ([n], "acos"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AcosOp  ix   )) state
    ([n], "atan"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AtanOp  ix   )) state
    ([n], "sinh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType SinhOp  ix   )) state
    ([n], "cosh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CoshOp  ix   )) state
    ([n], "tanh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType TanhOp  ix   )) state
    ([n], "asinh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AsinhOp ix   )) state
    ([n], "acosh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AcoshOp ix   )) state
    ([n], "atanh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AtanhOp ix   )) state
    ([n], "ceil"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CeilOp  ix   )) state
    ([n], "floor" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType FloorOp ix   )) state
    ([n], "round" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType RoundOp ix   )) state

    ([n], "const" , [c@(PI_Const _ cid)])                       -> appendEither (onRight (\x -> ConstNode n UndefType x) (getConstId syms c)) state

    ([n]  , "zip"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] ZipHF   [ix, jx] [])) state
    ([m,n], "unzip" , [(PI_Int ips ix)])                          -> appendEither (Right (HOFNode    [m,n] [] UnzipHF [ix    ] [])) state
    ([n]  , "revs"  , [(PI_Int ips ix)])                          -> appendEither (Right (HOFNode    [  n] [] RevsHF  [ix    ] [])) state
    ([n]  , "sort"  , [(PI_Int ips ix)])                          -> appendEither (Right (HOFNode    [  n] [] SortHF  [ix    ] [])) state
    ([n]  , "tail"  , [(PI_Int ips ix)])                          -> appendEither (Right (HOFNode    [  n] [] TailHF  [ix    ] [])) state
    ([n]  , "take"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] TakeHF  [ix, jx] [])) state
    ([n]  , "drop"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] DropHF  [ix, jx] [])) state
    ([n]  , "rept"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] ReptHF  [ix, jx] [])) state
    ([n]  , "concat", [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] ConctHF [ix, jx] [])) state
    ([n]  , "append", [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] AppndHF [ix, jx] [])) state

    ([n], "map"   , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] MapHF [ix])
    ([n], "sfold" , [f@(PI_Func p0 fn), (PI_Int p1 ix), (PI_Int p2 jx)]) -> hofmodel syms state [f] (HOFNode [n] [] SFoldHF [ix, jx])
    ([n], "pfold" , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] PFoldHF [ix])
    ([n], "sscan" , [f@(PI_Func p0 fn), (PI_Int p1 ix), (PI_Int p2 jx)]) -> hofmodel syms state [f] (HOFNode [n] [] SScanHF [ix, jx])
    ([n], "pscan" , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] PScanHF [ix])
    ([n], "filter" , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                -> hofmodel syms state [f] (HOFNode [n] [] FilterHF [ix])
    ([n], "any"   , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] AnyHF [ix])
    ([n], "all"   , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] AllHF [ix])
    ([n], "none"  , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] NoneHF [ix])
    ([n], "iter"  , [f@(PI_Func p0 fn), (PI_Int p1 ix), (PI_Int p2 jx)]) -> hofmodel syms state [f] (HOFNode [n] [] IterHF [ix, jx])

    (ns,  "phi"   , f@(PI_Func p0 fn):g@(PI_Func p1 gn):(PI_Int ips ix):xs) ->
      let fnid = getFnid syms f
          gnid = getFnid syms g
          pars = makeIntList xs
          ers = (getLefts pars) ++ (lefts [fnid]) ++ (lefts [gnid])
          ret = (PhiNode ns [] ix (justRight fnid) (justRight gnid) (getRights pars))
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)


    (ns , "call"  , f@(PI_Func ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns [] FnCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (ns , "prcall", f@(PI_Proc ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns [] PrCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (ns , "excall", f@(PI_Extn ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns [] ExCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (_  , _       , _                                         ) -> appendEither (Left  (IRErr p $ pack "Unrecognized operation.\n")) state

  where hofmodel :: IRSymbols -> ([IRErr], [Node]) -> [IRParseItem] -> ([FnId] -> Node) -> ([IRErr], [Node])
        hofmodel syms (errs, nods) fs makeNode =
          let fnids = L.map (getFnid syms) fs
              ers = lefts fnids
              ret = makeNode (rights fnids)
          in case ers of
              [] -> (errs,   ret:nods)
              er -> (er ++ errs, nods)


modelFuncNode syms state _ = state










buildFuncNodeModel :: FuncData -> FuncData
buildFuncNodeModel (FuncType fk its ots res wes hs nds fnid _ _ _) =
  let (fnds, fots, ct) = L.foldl (\(ns, os, c) nd -> (M.insert (c+1) nd ns,
                                                      insertMany os (L.zip (nodeOuts nd) $ L.repeat (c+1)),
                                                      c+1)) (M.empty, M.empty, 0) nds
  in (FuncType fk its ots res wes hs nds fnid fnds fots ct)
