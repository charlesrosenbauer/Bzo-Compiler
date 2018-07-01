module NodeOps where
import Data.Map as M
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










--calls :: FuncData -> [FnId]










--typeCheck :: FuncData -> [Int]










--deriveTypes :: FuncData -> Either [Int] FuncData










--isValid :: FuncData -> [Int]










opCt :: FuncData -> Int
opCt = nodeCount
