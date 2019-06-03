module Query where
import BzoTypes
import HigherOrder
import Builtins
import Data.Text
import Data.Int
import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Tuple as Tp
import Control.Parallel.Strategies
import Debug.Trace










getTVars :: BzoSyntax -> [(Text, BzoPos)]
getTVars (BzS_TyVar      p    tvar) = [(tvar, p)]
getTVars (BzS_Expr       _    expr) = L.concatMap getTVars expr
getTVars (BzS_Statement  _    expr) = getTVars expr
getTVars (BzS_Cmpd       _    expr) = L.concatMap getTVars expr
getTVars (BzS_Poly       _    expr) = L.concatMap getTVars expr
getTVars (BzS_FnTy       _   ax bx) = (getTVars ax) ++ (getTVars bx)
getTVars (BzS_Block      _    expr) = L.concatMap getTVars expr
getTVars (BzS_TypDef     _ ps _ df) = (getTVars ps) ++ (getTVars df)
getTVars (BzS_TyClassDef _ ps _ df) = (getTVars ps) ++ (L.concatMap getTVars df)
getTVars (BzS_FnTypeDef  _ ps _ df) = (getTVars ps) ++ (getTVars df)
getTVars (BzS_Calls      _      cs) = L.concatMap getTVars cs
getTVars (BzS_ArrayObj   _  _ expr) = getTVars expr
getTVars (BzS_FilterObj  _ obj  fs) = (getTVars obj) ++ (L.concatMap getTVars fs)
getTVars (BzS_CurryObj   _ obj  ps) = (getTVars obj) ++ (L.concatMap getTVars ps)
getTVars (BzS_LispCall   _ fn expr) = (getTVars fn)  ++ (L.concatMap getTVars expr)
getTVars _                          = []










getTypes :: BzoSyntax -> [(Text, BzoPos)]
getTypes (BzS_TyId       p     var) = [(var, p)]
getTypes (BzS_Expr       _    expr) = L.concatMap getTypes expr
getTypes (BzS_Statement  _    expr) = getTypes expr
getTypes (BzS_Cmpd       _    expr) = L.concatMap getTypes expr
getTypes (BzS_Poly       _    expr) = L.concatMap getTypes expr
getTypes (BzS_FnTy       _   ax bx) = (getTypes ax) ++ (getTypes bx)
getTypes (BzS_Block      _    expr) = L.concatMap getTypes expr
getTypes (BzS_TypDef     _ ps _ df) = (getTypes ps) ++ (getTypes df)
getTypes (BzS_TyClassDef _ ps _ df) = (getTypes ps) ++ (L.concatMap getTypes df)
getTypes (BzS_FnTypeDef  _ ps _ df) = (getTypes ps) ++ (getTypes df)
getTypes (BzS_FunDef     _ i _ o x) = (getTypes i)  ++ (getTypes o)  ++ (getTypes x)
getTypes (BzS_Calls      _      cs) = L.concatMap getTypes cs
getTypes (BzS_ArrayObj   _  _ expr) = getTypes expr
getTypes (BzS_FilterObj  _ obj  fs) = (getTypes obj) ++ (L.concatMap getTypes fs)
getTypes (BzS_CurryObj   _ obj  ps) = (getTypes obj) ++ (L.concatMap getTypes ps)
getTypes (BzS_MapObj     _    expr) = (getTypes expr)
getTypes (BzS_Lambda     _ ps expr) = (getTypes ps)  ++ (getTypes expr)
getTypes (BzS_LispCall   _ fn expr) = (getTypes fn)  ++ (L.concatMap getTypes expr)
getTypes (BzS_ExTypObj   p ty    _) = [(ty, p)]
getTypes _                          = []










getBuiltins :: BzoSyntax -> [(Text, BzoPos)]
getBuiltins (BzS_BId        p     var) = [(var, p)]
getBuiltins (BzS_BTId       p     var) = [(var, p)]
getBuiltins (BzS_Expr       _    expr) = L.concatMap getBuiltins expr
getBuiltins (BzS_Statement  _    expr) = getBuiltins expr
getBuiltins (BzS_Cmpd       _    expr) = L.concatMap getBuiltins expr
getBuiltins (BzS_Poly       _    expr) = L.concatMap getBuiltins expr
getBuiltins (BzS_FnTy       _   ax bx) = (getBuiltins ax) ++ (getBuiltins bx)
getBuiltins (BzS_Block      _    expr) = L.concatMap getBuiltins expr
getBuiltins (BzS_TypDef     _ ps _ df) = (getBuiltins ps) ++ (getBuiltins df)
getBuiltins (BzS_TyClassDef _ ps _ df) = (getBuiltins ps) ++ (L.concatMap getBuiltins df)
getBuiltins (BzS_FnTypeDef  _ ps _ df) = (getBuiltins ps) ++ (getBuiltins df)
getBuiltins (BzS_FunDef     _ i _ o x) = (getBuiltins i)  ++ (getBuiltins o)  ++ (getBuiltins x)
getBuiltins (BzS_Calls      _      cs) = L.concatMap getBuiltins cs
getBuiltins (BzS_ArrayObj   _  _ expr) = getBuiltins expr
getBuiltins (BzS_FilterObj  _ obj  fs) = (getBuiltins obj) ++ (L.concatMap getBuiltins fs)
getBuiltins (BzS_CurryObj   _ obj  ps) = (getBuiltins obj) ++ (L.concatMap getBuiltins ps)
getBuiltins (BzS_MapObj     _    expr) = (getBuiltins expr)
getBuiltins (BzS_Lambda     _ ps expr) = (getBuiltins ps)  ++ (getBuiltins expr)
getBuiltins (BzS_LispCall   _ fn expr) = (getBuiltins fn)  ++ (L.concatMap getBuiltins expr)
getBuiltins _                          = []










getNamespaces :: BzoSyntax -> [(Text, BzoPos)]
getNamespaces (BzS_ExTypObj   p _    ns) = [(ns, p)]
getNamespaces (BzS_ExFunObj   p _    ns) = [(ns, p)]
getNamespaces (BzS_Expr       _    expr) = L.concatMap getNamespaces expr
getNamespaces (BzS_Statement  _    expr) = getNamespaces expr
getNamespaces (BzS_Cmpd       _    expr) = L.concatMap getNamespaces expr
getNamespaces (BzS_Poly       _    expr) = L.concatMap getNamespaces expr
getNamespaces (BzS_FnTy       _   ax bx) = (getNamespaces ax) ++ (getNamespaces bx)
getNamespaces (BzS_Block      _    expr) = L.concatMap getNamespaces expr
getNamespaces (BzS_TypDef     _ ps _ df) = (getNamespaces ps) ++ (getNamespaces df)
getNamespaces (BzS_TyClassDef _ ps _ df) = (getNamespaces ps) ++ (L.concatMap getNamespaces df)
getNamespaces (BzS_FnTypeDef  _ ps _ df) = (getNamespaces ps) ++ (getNamespaces df)
getNamespaces (BzS_FunDef     _ i _ o x) = (getNamespaces i)  ++ (getNamespaces o)  ++ (getNamespaces x)
getNamespaces (BzS_Calls      _      cs) = L.concatMap getNamespaces cs
getNamespaces (BzS_ArrayObj   _  _ expr) = getNamespaces expr
getNamespaces (BzS_FilterObj  _ obj  fs) = (getNamespaces obj) ++ (L.concatMap getNamespaces fs)
getNamespaces (BzS_CurryObj   _ obj  ps) = (getNamespaces obj) ++ (L.concatMap getNamespaces ps)
getNamespaces (BzS_MapObj     _    expr) = (getNamespaces expr)
getNamespaces (BzS_Lambda     _ ps expr) = (getNamespaces ps)  ++ (getNamespaces expr)
getNamespaces (BzS_LispCall   _ fn expr) = (getNamespaces fn)  ++ (L.concatMap getNamespaces expr)
getNamespaces _                          = []










getNamespaceSet :: (Show a) => BzoFileModel a -> S.Set Text
getNamespaceSet (BzoFileModel _ _ _ _ is ls ias las) = S.fromList $ (is ++ ls) ++ (L.map snd (ias ++ las))










isType :: Definition -> Bool
isType (TypeSyntax    _ _ _) = True
isType (TyClassSyntax _ _ _) = True
isType (TypeDef     _ _ _ _) = True
isType (TyClassDef  _ _ _ _) = True
isType _ = False

-- This should probably be renamed?
isStructType :: Definition -> Bool
--isStructType (TypeSyntax    _ _ _) = True
--isStructType (TyClassSyntax _ _ _) = True
isStructType (TypeDef     _ _ _ _) = True
isStructType (TyClassDef  _ _ _ _) = True
isStructType _ = False



isTyDef :: Definition -> Bool
isTyDef (TypeSyntax _ _ _) = True
isTyDef (TypeDef  _ _ _ _) = True
isTyDef _ = False



isTyClass :: Definition -> Bool
isTyClass (TypeSyntax    _ _ _) = True
isTyClass (TyClassSyntax _ _ _) = True
isTyClass _ = False
