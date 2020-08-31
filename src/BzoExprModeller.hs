{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2020 Charles Rosenbauer

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
import Builtins
import Core
import Query
import AST
import Core
import Error
import Data.Text
import Data.List   as L
import Data.Either as E
import Data.Maybe  as Mb
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







data ExprType = ExprType{
  xt_type :: !Type,
  xt_head :: !TypeHeader }

data Expr
  = IntLit !Integer
  | FltLit !Double
  | StrLit !Text
  | Nil
  | FunLit ![Int64]
  | TypLit ! Int64
  | VarLit ! Int64
  | MVrLit ! Int64
  | Cmpd   ![Expr]
  | Poly   ![Expr]
  | Expr   ![Expr]
  | Lisp   ! Expr  ![Expr]
  | MapExp ! Expr
  | Let    ![([Int64], Expr, [Int64])]  !ScopeData
  | Filter ! Expr  ![Expr]
  | Lambda ! Expr  ! Expr
  | MkLmda ! Int64 ![Expr]          -- Don't remember this. Make lambda?
  | ClLmda ! Int64 ![Expr] ![Expr]  -- Don't remember this. Call lambda?
  | Wild
  | BITyp  !Int64
  | BIFnc  !Int64
  | Func   ! Expr  !Expr !Expr !ScopeData
  | Undef

showExpr :: Expr -> String
showExpr (IntLit    i) = "[INT: "  ++ (show   i) ++ "]"
showExpr (FltLit    f) = "[FLT: "  ++ (show   f) ++ "]"
showExpr (StrLit    s) = "[STR: "  ++ (unpack s) ++ "]"
showExpr (Nil        ) = "[NIL]"
showExpr (FunLit   fs) = "[FN: "   ++ (show fs) ++ "]"
showExpr (TypLit    t) = "[TY: "   ++ (show t)  ++ "]"
showExpr (VarLit    v) = "[VR: "   ++ (show v)  ++ "]"
showExpr (MVrLit    m) = "[~VR: "  ++ (show m)  ++ "]"
showExpr (Cmpd     xs) = "[CMPD: " ++ (show xs) ++ "]"
showExpr (Poly     xs) = "[POLY: " ++ (show xs) ++ "]"
showExpr (Expr     xs) = "[EXPR: " ++ (show xs) ++ "]"
showExpr (Lisp   x xs) = "[LISP: " ++ (show x) ++ " <-- " ++ (show xs) ++ "]"
showExpr (MapExp    x) = "[MAP: "  ++ (show x) ++  "]"
showExpr (Let   vs sc) =
  "[LET:\n    SC:" ++ (show sc) ++ "\n    VS:\n" ++
  (L.concatMap (\(ins, x, exs) -> "        [" ++ (show ins) ++ " >> " ++ (show x) ++ " >> " ++ (show exs) ++ "\n") vs) ++ "]"
showExpr (Filter f xs) = "[FILT: " ++ (show f) ++ " : " ++ (show xs) ++ "]"
showExpr (Lambda v  x) = "[λ " ++ (show v) ++ " . " ++ (show x) ++ "]"

showExpr (Wild       ) = "[_]"
showExpr (BITyp     t) = "[BITY: " ++ (show t) ++ "]"
showExpr (BIFnc     f) = "[BIFN: " ++ (show f) ++ "]"
showExpr (Func ins fn exs sc) = "[FUNC:\nFN:" ++ (show ins) ++ "\nIN:" ++ (show fn) ++ "\nEX:" ++ (show exs) ++ "\nSC:" ++ (show sc) ++ "]"
showExpr (Undef)       = "[X]"
showExpr _ = "[???]"
instance Show Expr where show = showExpr


data FunctionModel = FunctionModel{
    fm_fnid   :: !Int64,
    fm_fname  :: !Text,
    fm_host   :: !Text,
    fm_type   :: !(TypeHeader, Type),
    fm_exprs  :: ![(Expr, ScopeData)] }

data ScopeData = ScopeData{
  scopestack  :: ![(M.Map Text Int64)],
  varmap      :: !(M.Map Int64 (TypeHeader, Type)),
  vartop      :: !Int64 }
  deriving Show


showFnModel :: FunctionModel -> String
showFnModel (FunctionModel fnid fname host ty exprs) =
  "[" ++ (unpack fname) ++ "<" ++ (show fnid) ++ ">@" ++ (unpack host) ++
      "\n  TYPE ::  " ++ (show ty) ++
      "\n  EXPR ::  " ++ (show exprs) ++ "\n]\n"
instance Show FunctionModel where show = showFnModel





findVariable :: ScopeData -> Text -> Maybe Int64
findVariable (ScopeData []         _    _  ) vid = Nothing
findVariable (ScopeData (smap:stk) vars top) vid =
  case M.lookup vid smap of
    Nothing -> findVariable (ScopeData stk vars top) vid
    Just x  -> Just x



makeVariable :: ScopeData -> Text -> (Int64, ScopeData)
makeVariable sd@(ScopeData []         vars top) vid = (top+1, ScopeData ((M.insert vid (top+1) M.empty):[]) vars (top+1))
makeVariable sd@(ScopeData (smap:stk) vars top) vid =
  let
      found :: Maybe Int64
      found =  findVariable sd vid

      sd'   :: ScopeData
      sd'   =  ScopeData ((M.insert vid (top+1) smap):stk) vars (top+1)

  in case found of
      Nothing -> (top+1, sd')
      Just x  -> (x    , sd )




modelExprs :: FileTable -> DefinitionTable -> ScopeData -> [BzoSyntax] -> Either [BzoErr] ([Expr], ScopeData)
modelExprs ft dt sd []     = Right ([], sd)
modelExprs ft dt sd (x:xs) =
  let
      outs     :: Either [BzoErr] (Expr, ScopeData)
      outs     = modelExpr ft dt sd x

      nextOuts :: ScopeData -> Either [BzoErr] ([Expr], ScopeData)
      nextOuts = (\s -> modelExprs ft dt s xs)

  in case outs of
      Left  err       -> Left err
      Right (ex, sd') ->
        case nextOuts sd' of
          Left  err         -> Left  err
          Right (exs, sd'') -> Right (ex:exs, sd'')


checkCmpd :: FileTable -> DefinitionTable -> ScopeData -> (BzoPos, [BzoSyntax]) -> (BzoPos, [BzoSyntax]) -> [BzoErr]
checkCmpd ft dt sd (p0, xs) (p1, ys) =
  let
      xlen = L.length xs
      ylen = L.length ys

      sameLen :: [BzoErr]
      sameLen = ife (xlen == ylen) [] [TypeErr p1 $ pack $
        "Expected " ++ (show xlen) ++ " elements in compound expression, found " ++ (show ylen) ++ "."]


  in sameLen



checkExprDef :: FileTable -> DefinitionTable -> ScopeData -> [BzoSyntax] -> BzoPos -> Either [BzoErr] (Expr, ScopeData)
checkExprDef ft dt sd xs p =
  let
      ins  :: BzoSyntax
      ins  = L.head xs

      exs  :: BzoSyntax
      exs  = L.last xs

      body :: [BzoSyntax]
      body = L.init $ L.tail xs


  in Left []



modelExpr :: FileTable -> DefinitionTable -> ScopeData -> BzoSyntax -> Either [BzoErr] (Expr, ScopeData)
modelExpr ft dt sd (BzS_Int  p i   ) = Right (IntLit i, sd)
modelExpr ft dt sd (BzS_Flt  p f   ) = Right (FltLit f, sd)
modelExpr ft dt sd (BzS_Str  p s   ) = Right (StrLit s, sd)
modelExpr ft dt sd (BzS_Nil  p     ) = Right (Nil     , sd)
modelExpr ft dt sd (BzS_Wildcard p ) = Right (Wild    , sd)
modelExpr ft dt sd (BzS_BId  p x   ) =
  case (isBuiltinFunc x) of
    0 -> Left [ModelErr p $ pack $ (unpack x) ++ " is not a valid builtin function."]
    n -> Right (BIFnc n, sd)

modelExpr ft dt sd (BzS_BTId p x   ) =
  case (isBuiltinType x) of
    0 -> Left [ModelErr p $ pack $ (unpack x) ++ " is not a valid builtin type."]
    n -> Right (BITyp n, sd)

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

modelExpr ft dt sd (BzS_TyId p x   ) =
  let
      ids :: [Int64]
      ids = resolveTyId dt ft x

      vid :: Int64
      sd' :: ScopeData
      (vid, sd') = makeVariable sd x

  in case ids of
      []   -> (Left  [ModelErr p $ pack $ (unpack x) ++ " is not a defined type."])
      [xs] -> (Right (TypLit xs, sd ))

modelExpr ft dt sd (BzS_MId p x    ) =
  let
      vid :: Int64
      sd' :: ScopeData
      (vid, sd') = makeVariable sd x
  in Right (MVrLit vid, sd')

modelExpr ft dt sd (BzS_Expr p xs) =
  let
      modEx :: Either [BzoErr] ([Expr], ScopeData)
      modEx = modelExprs ft dt sd (L.reverse xs)
  
      -- TODO: Include typechecking
      

  in case modEx of
      Left errs        -> Left $ [ModelErr p $ pack "Invalid expression"] ++ errs
      Right (xs', sd') -> Right  (Expr xs', sd')


modelExpr ft dt sd _ = Left []
{-


modelExpr ft dt sd (BzS_Cmpd p xs) =
  case modelExprs ft dt sd (L.reverse xs) of
    Left errs        -> Left $ [ModelErr p $ pack "Invalid compound expression."] ++ errs
    Right (xs', sd') -> Right  (Cmpd xs', sd')

modelExpr ft dt sd (BzS_Poly p xs) =
  case modelExprs ft dt sd (L.reverse xs) of
    Left errs        -> Left $ [ModelErr p $ pack "Invalid polymorphic expression."] ++ errs
    Right (xs', sd') -> Right  (Poly xs', sd')

modelExpr ft dt sd (BzS_Expr p xs) =
  case modelExprs ft dt sd (L.reverse xs) of
    Left errs        -> Left $ [ModelErr p $ pack "Invalid expression."] ++ errs
    Right (xs', sd') -> Right  (Expr xs', sd')

modelExpr ft dt sd (BzS_LispCall p f xs) =
  case modelExprs ft dt sd (f:(L.reverse xs)) of
    Left errs           -> Left  $ [ModelErr p $ pack "Invalid prefix expression."] ++ errs
    Right (f':xs', sd') -> Right (Lisp f' xs', sd')

-- Not quite right, cs should be modelled as types
modelExpr ft dt sd (BzS_FilterObj p t cs) =
  case modelExprs ft dt sd (t:cs) of
    Left errs           -> Left  $ [ModelErr p $ pack "Invalid constraint."] ++ errs
    Right (t':cs', sd') -> Right (Filter t' cs', sd')

modelExpr ft dt sd (BzS_MapObj p expr) =
  case modelExpr ft dt sd expr of
    Left errs        -> Left $ [ModelErr p $ pack "Invalid map expression."] ++ errs
    Right (x',  sd') -> Right  (MapExp x', sd')

modelExpr ft dt sd (BzS_Statement p expr) = modelExpr ft dt sd expr

-- Parameters are not modelled correctly yet
modelExpr ft dt sd (BzS_FunDef p is f os df) =
  case modelExprs ft dt sd (is:os:df:[]) of
    Left errs                  -> Left  $ [ModelErr p $ pack "Invalid function header."] ++ errs
    Right ([is',os',df'], sd') -> Right (Func df' is' os' sd', sd')

modelExpr ft dt sd (BzS_Undefined p) = Right (Undef, sd)

modelExpr ft dt sd (BzS_Lambda p ps expr) =
  let

  in Right (Undef, sd)

-- This is clearly wrong
modelExpr ft dt sd@(ScopeData stk vm top) (BzS_Block p xs) =
  let
      sd' :: ScopeData
      sd' = (ScopeData ((M.empty):stk) vm top)
  in case modelExprs ft dt sd' (L.reverse xs) of
    Left errs         -> Left $ [ModelErr p $ pack "Invalid block expression."] ++ errs
    Right (xs', sd'') -> Right  (Let (L.map (\x -> ([], x, [])) xs') sd'', sd'')

modelExpr ft dt sd x = Left [ModelErr (pos x) $ pack $ "Unmodelable expression:" ++ (show x)]
-}





modelFunction :: SymbolTable -> DefinitionTable -> Int64 -> Definition -> Either [BzoErr] FunctionModel
modelFunction st dt@(DefinitionTable defs files ids _) fnix (FuncDef p fnid host thead ftyp fndefs) =
  let
      ft :: FileTable
      ft = Mb.fromJust $ getFileTable st host

      sd :: ScopeData
      sd = ScopeData [] M.empty 0

      exprs :: [(Expr, ScopeData)]
      errs  :: [[BzoErr]]
      (errs, exprs) = E.partitionEithers $ L.map (modelExpr ft dt sd) fndefs

  in case (L.concat errs) of
      [] -> Right $ FunctionModel fnix fnid host (thead, ftyp) exprs
      er -> Left  er




modelFunctions :: SymbolTable -> DefinitionTable -> Either [BzoErr] (M.Map Int64 FunctionModel)
modelFunctions st dt@(DefinitionTable defs files ids _) =
  let
      fnouts :: [(Int64, Either [BzoErr] FunctionModel)]
      fnouts = L.map (\(i, f) -> (i, modelFunction st dt i f)) $ L.filter (\(_, d) -> isFunc d) $ M.assocs defs

      fnrets :: Either [BzoErr] [FunctionModel]
      fnids  :: [Int64]
      (fnids, fnrets) = (\(is, outs) -> (is, allPass outs)) $ L.unzip fnouts
  in case fnrets of
      Left err     -> Left   err
      Right fnmods -> Right (M.fromList $ L.zip fnids fnmods)




modelProgram :: DefinitionTable -> Either [BzoErr] (M.Map Int64 FunctionModel, DefinitionTable)
modelProgram dt =
  let
      syms :: SymbolTable
      syms = makeSymbolTable dt
  in applyRight (\x -> (x, dt)) $ modelFunctions syms dt


{-
  Typechecking rules:

  {
    [CMPD x: a, b] [CMPD y: c, d]

    (len x) == (len y)
    a -> c
    b -> d
  }

  {
    x [POLY y: a, b]

    x -> a
    x -> c
  }

  {
    [EXPR: a, b, c]

    a -> b
    b -> c
  }

  {
    [LISP: f, a, b]

    [CMPD: a, b] -> f
  }

  {
    x [LISP: f, _, a]

    [CMPD: x, a] -> f
  }
-}


{-
printProgram :: DefinitionTable -> Either [BzoErr] String
printProgram (DefinitionTable defs files ids _) =
  let


  in-}
