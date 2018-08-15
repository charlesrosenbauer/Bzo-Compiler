module IRTypeModel where
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










modelType :: [IRParseItem] -> IRSymbols -> ([IRErr], [TypeData])
modelType [] syms = ([], [])
modelType (t@(PI_TyDef  p tyid parnum def):irs) syms =
  let tyid' = (typeSymbols syms) ! tyid

      -- Model Contents
      (er0, nds) = L.foldl (modelTypeNodes syms) ([], []) def

      -- Model Attributes

      -- Validate Contents
      er1 = L.concatMap (validateTypeNode t) nds
      ns  = L.map tidx nds
      ns' = L.nub $ ns
      er2 = if ns /= ns'
              then [(IRErr p $ pack $ "Type contains repeated element ids: " ++ (show $ L.map (\x -> "#" ++ (show x)) (ns L.\\ ns')) ++ "\n")]
              else []

      -- Model the rest of the list
      (ers, tys) = modelType irs syms
  in (er0 ++ er1 ++ er2 ++ ers, ((TypeData PureTy nds 0 [] tyid'):tys))










validateTypeNode :: IRParseItem -> TypeNode -> [IRErr]
validateTypeNode (PI_TyDef p tyid parnum def) (ElemNode n _ _) = ife ((0 Prelude.<= n) && (n < parnum))
                                                                     []
                                                                     [IRErr p $ pack $ "Type definition contains invalid element node number: " ++ (show n)]
validateTypeNode _ _ = []  -- For now











modelTypeNodes :: IRSymbols -> ([IRErr], [TypeNode]) -> IRParseItem -> ([IRErr], [TypeNode])
modelTypeNodes syms state n@(PI_Node p ns op pars) =
  case (ns, unpack op, L.reverse pars) of
    ([n], "element" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (ElemNode     n 0        ) (makeTypeRef syms t)) state
    ([n], "contlist", [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n ListCont ) (makeTypeRef syms t)) state
    ([n], "contset" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n SetCont  ) (makeTypeRef syms t)) state
    ([n], "contbset", [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n BSetCont ) (makeTypeRef syms t)) state
    ([n], "contrrb" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n RRBCont  ) (makeTypeRef syms t)) state
    ([n], "contbox" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n BoxCont  ) (makeTypeRef syms t)) state
    ([n], "contq"   , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n QCont    ) (makeTypeRef syms t)) state
    ([n], "contstk" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n StkCont  ) (makeTypeRef syms t)) state
    ([n], "contheap", [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n HeapCont ) (makeTypeRef syms t)) state

    ([n], "contdict", [t0@(PI_Type _ _ _), t1@(PI_Type _ _ _)])   -> model2Node syms state t0 t1 (Contain2Node n DictCont)
    ([n], "conthmap", [t0@(PI_Type _ _ _), t1@(PI_Type _ _ _)])   -> model2Node syms state t0 t1 (Contain2Node n HMapCont)
    ([n], "contavl" , [t0@(PI_Type _ _ _), t1@(PI_Type _ _ _)])   -> model2Node syms state t0 t1 (Contain2Node n AVLCont )

    ([n], "implmap"   , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplMap)
    ([n], "implsplit" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSplit)
    ([n], "implsfold" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSFold)
    ([n], "implpfold" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplPFold)
    ([n], "implsscan" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSScan)
    ([n], "implpscan" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplPScan)
    ([n], "implzip"   , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplZip)
    ([n], "impluzip"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplUZip)
    ([n], "implnext"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplNext)
    ([n], "implindex" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplIndex)
    ([n], "implhash"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplHash)
    ([n], "impleq"    , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplEq)
    ([n], "implcmp"   , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplCmp)
    ([n], "implserl"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSerl)    -- Serialize
    ([n], "impldserl" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplDSerl)   -- Deserialize
    ([n], "implsize"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSize)

    (_  , _           , _)                 -> appendPair ([IRErr p $ pack "Invalid Type Node.\n"], []) state

  where modelImpl :: IRSymbols -> ([IRErr], [TypeNode]) -> IRParseItem -> (FnId -> TypeNode) -> ([IRErr], [TypeNode])
        modelImpl syms (errs, nods) f makeNode =
          let fnid = getFnid syms f
              ers  = lefts [fnid]
              ret  = makeNode (justRight fnid)
          in case ers of
              [] -> (errs,   ret:nods)
              er -> (er ++ errs, nods)


        model2Node :: IRSymbols -> ([IRErr], [TypeNode]) -> IRParseItem -> IRParseItem -> (TypeRef -> TypeRef -> TypeNode) -> ([IRErr], [TypeNode])
        model2Node syms (errs, nods) t0 t1 makeNode =
          let t0' = makeTypeRef syms t0
              t1' = makeTypeRef syms t1
          in case (t0', t1') of
              (Right a, Right b) -> (errs, (makeNode a b):nods)
              (Right a, Left  b) -> (b:errs,              nods)
              (Left  a, Right b) -> (a:errs,              nods)
              (Left  a, Left  b) -> (a:b:errs,            nods)

modelTypeNodes syms state _ = state








initialSymbolTable :: IRSymbols
initialSymbolTable =
  let typeList :: [(Text, Int)]
      typeList = L.map (\(a, b) -> (pack a, fromIntegral b)) (
                 [("I8",   1), ("I16",  2), ("I32",  3), ("I64",  4),
                  ("U8",   5), ("U16",  6), ("U32",  7), ("U64",  8),
                  ("N16",  9), ("N32", 10), ("N64", 11),
                  ("F16", 12), ("F32", 13), ("F64", 14),
                  ("ASCII", 15), ("UTF8", 16), ("UTF16", 17), ("UTF32", 18),
                  ("Char",  19), ("UChar",20), ("Bl",    21),
                  ("I8v2",  22), ("I16v2", 23), ("I32v2", 24), ("I64v2", 25),
                  ("U8v2",  26), ("U16v2", 27), ("U32v2", 28), ("U64v2", 29),
                  ("N16v2", 30), ("N32v2", 31), ("N64v2", 32),
                  ("F16v2", 33), ("F32v2", 34), ("F64v2", 35),
                  ("I8v4",  36), ("I16v4", 37), ("I32v4", 38), ("I64v4", 39),
                  ("U8v4",  40), ("U16v4", 41), ("U32v4", 42), ("U64v4", 43),
                  ("N16v4", 44), ("N32v4", 45), ("N64v4", 46),
                  ("F16v4", 47), ("F32v4", 48), ("F64v4", 49),
                  ("I8v8",  50), ("I16v8", 51), ("I32v8", 52), ("I64v8", 53),
                  ("U8v8",  54), ("U16v8", 55), ("U32v8", 56), ("U64v8", 57),
                  ("N16v8", 58), ("N32v8", 59), ("N64v8", 60),
                  ("F16v8", 61), ("F32v8", 62), ("F64v8", 63),
                  ("I8v16", 64), ("I16v16",65), ("I32v16",66), ("I64v16",67),
                  ("U8v16", 68), ("U16v16",69), ("U32v16",70), ("U64v16",71),
                  ("N16v16",72), ("N32v16",73), ("N64v16",74),
                  ("F16v16",75), ("F32v16",76), ("F64v16",77)])
      typeList' = L.map swap typeList

  in IRSymbols M.empty M.empty (M.fromList typeList) (M.fromList typeList') M.empty M.empty 78
