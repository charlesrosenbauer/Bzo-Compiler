module NodeOps where
import Data.Map as M
import Data.List as L
import IRTypes










isRoot :: Node -> Bool
isRoot (ParNode _ _) = True
isRoot _             = False










isRet :: Node -> Bool
isRet (RetNode _ _ _) = True
isRet _               = False










getTypes :: Node -> [TypeRef]
getTypes (CallNode _ ts _ _ _  ) = ts
getTypes (HOFNode  _ ts _ _ _  ) = ts
getTypes (PhiNode  _ ts _ _ _ _) = ts
getTypes (CondNode _ _ _ _)      = [typeBool]
getTypes nd = [ntyp nd]










getNodeFromRet :: FuncData -> Int -> Maybe Node
getNodeFromRet fdata ret = (M.lookup ret (fnFromOuts fdata)) >>= (\x -> M.lookup x (fnNodes fdata))










--dce :: FuncData -> FuncData









--inline :: FuncData -> [FnId] -> FuncData










calls :: FuncData -> [FnId]
calls fd = concatMap getCalls $ M.elems $ fnNodes fd
  where getCalls :: Node -> [FnId]
        getCalls (CallNode _ _ _ f _  ) = [f]
        getCalls (PhiNode  _ _ _ f g _) = [f,g]
        getCalls (HOFNode  _ _ _ _ fs ) =  fs
        getCalls _ = []










--typeCheck :: FuncData -> [Int]










isValid :: FuncData -> [Int]
isValid fdata =
  let allIns = L.nub $ L.concat $ L.map getIns   $ funcNodes fdata
      allOts = L.nub $ L.concat $ L.map nodeOuts $ funcNodes fdata
  in L.intersect (allIns L.\\ allOts) allIns










opCt :: FuncData -> Int
opCt = nodeCount
