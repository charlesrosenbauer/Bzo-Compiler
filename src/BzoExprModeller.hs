{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2019 Charles Rosenbauer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.-}

module BzoExprModeller where
import BzoTypes
import HigherOrder
import BzoChecker
import Core
import Query
import AST
import Core
import Error
import Data.Text
import Data.List as L
import Unsafe.Coerce
import Data.Int
import Text.Printf
import Data.Map.Strict as M










getFloatHex :: Double -> String
getFloatHex f =
  let bits :: Int64
      bits = unsafeCoerce f

  in  printf "%08x" bits










printFunction :: BzoSyntax -> String
printFunction (BzS_Int           _ i) = "i" ++ (show i) ++ " "
printFunction (BzS_Flt           _ f) = "f" ++ (getFloatHex f) ++ " "
printFunction (BzS_Nil             _) = "- "
printFunction (BzS_Str           _ s) = "\"" ++ (unpack s) ++ "\" "     -- May cause problems when string contains escape codes
--printFunction (BzS_TyId          _ t) =  -- get type id mapping
--printFunction (BzS_Id            _ f) =  -- determine if id is function or type. If function, account for overloading
--printFunction (BzS_MId           _ x) =  -- get var id mapping
printFunction (BzS_Wildcard        _) = "_ "
printFunction (BzS_Cmpd         _ xs) = "(x303 " ++ (L.concatMap printFunction xs) ++ ") "
printFunction (BzS_Poly         _ xs) = "(x304 " ++ (L.concatMap printFunction xs) ++ ") "
printFunction (BzS_Statement     _ x) = printFunction x
printFunction (BzS_Expr         _ xs) = "(x300 " ++ (L.concatMap printFunction xs) ++ ") "









data Expr
  = IntLit !Integer
  | FltLit !Double
  | StrLit !Text
  | Nil
  | FunLit ![Int64]
  | TypLit !Int64
  | VarLit !Int64
  | MVrLit !Int64
  | Cmpd   ![Expr]
  | Poly   ![Expr]
  | Expr   ![Expr]
  | Lisp   ![Expr]
  | Let    ![([Int64], Expr, [Int64])]  !ScopeData
  | Wild
  | BITyp  !Int64
  | BIFnc  !Int64


data FunctionModel = FunctionModel{
    fm_fnid   :: !Int64,
    fm_fname  :: !Text,
    fm_vars   ::  M.Map Int64 (TypeHeader, Type),
    fm_varct  :: !Int64,
    fm_expr   :: !Expr }

data ScopeData = ScopeData{
  scopestack  :: ![(M.Map Text Int64)],
  varmap      :: !(M.Map Int64 (TypeHeader, Type)),
  vartop      :: !Int64 }









findVariable :: ScopeData -> Text -> Maybe Int64
findVariable (ScopeData []         _    _  ) vid = Nothing
findVariable (ScopeData (smap:stk) vars top) vid =
  case M.lookup vid smap of
    Nothing -> findVariable (ScopeData stk vars top) vid
    Just x  -> Just x



makeVariable :: ScopeData -> Text -> (Int64, ScopeData)
makeVariable sd@(ScopeData (smap:stk) vars top) vid =
  let
      found :: Maybe Int64
      found =  findVariable sd vid

      sd'   :: ScopeData
      sd'   =  ScopeData ((M.insert vid (top+1) smap):stk) vars (top+1)

  in case found of
      Nothing -> (top+1, sd')
      Just x  -> (x    , sd )




modelExpr :: FileTable -> DefinitionTable -> ScopeData -> BzoSyntax -> Either [BzoErr] (Expr, ScopeData)
modelExpr ft dt sd (BzS_Int  p i   ) = Right (IntLit i, sd)
modelExpr ft dt sd (BzS_Flt  p f   ) = Right (FltLit f, sd)
modelExpr ft dt sd (BzS_Str  p s   ) = Right (StrLit s, sd)
modelExpr ft dt sd (BzS_Nil  p     ) = Right (Nil     , sd)
modelExpr ft dt sd (BzS_Wildcard p ) = Right (Wild    , sd)
--modelExpr ft dt sd (BzS_BId  p x   ) = Right (BIFnc  x, sd)
--modelExpr ft dt sd (BzS_BTId p x   ) = Right (BITyp  x, sd)
modelExpr ft dt sd (BzS_Id   p x   ) =
  let
      ids :: [Int64]
      ids = resolveFnId dt ft x

      vid :: Int64
      sd' :: ScopeData
      (vid, sd') = makeVariable sd x

  in case ids of
      [] -> (Right (VarLit vid, sd'))
      xs -> (Right (FunLit ids, sd ))

modelExpr ft dt sd (BzS_MId p x    ) =
  let
      vid :: Int64
      sd' :: ScopeData
      (vid, sd') = makeVariable sd x
  in Right (MVrLit vid, sd')




modelFunction :: DefinitionTable -> Definition -> Either [BzoErr] FunctionModel
modelFunction dt@(DefinitionTable defs files ids _) (FuncDef p fnid host thead ftyp fndefs) =
  let
      syms :: SymbolTable
      syms = makeSymbolTable dt
  in Left []





{-
printProgram :: DefinitionTable -> Either [BzoErr] String
printProgram (DefinitionTable defs files ids _) =
  let


  in-}
