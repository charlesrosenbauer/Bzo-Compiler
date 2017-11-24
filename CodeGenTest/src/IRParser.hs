module IRParser where
import IRLexer
import Data.List as L
import Data.Text










data IRParseItem
  = PI_FnHeader IRPos Text Int Int
  | PI_FnDef    IRPos Text Int Int [IRParseItem]
  | PI_TyHeader IRPos Text Int
  | PI_TyDef    IRPos Text Int [IRParseItem]
  | PI_PrHeader IRPos Text Int Int Text
  | PI_PrDef    IRPos Text Int Int Text [IRParseItem]
  | PI_ExHeader IRPos Text Int Int
  | PI_ExDef    IRPos Text Int Int [IRParseItem]
  | PI_NL       IRPos
  | PI_Ptr      IRPos Int
  | PI_Arr      IRPos Int
  | PI_MultiArr IRPos [Int]
  | PI_Type     IRPos [Int] Int Text
  | PI_Node     IRPos Int Text IRParseItem
  | PI_NS       IRPos [Int]
  | PI_Nodes    IRPos [IRParseItem]
  | PI_Def      IRPos IRParseItem
  | PI_Defs     IRPos [IRParseItem]
  | PI_Const    IRPos Text
  | PI_ConstInt IRPos Text Int
  | PI_Consstktr IRPos Text Text
  | PI_Str      IRPos Text
  | PI_Int      IRPos Int
  | PI_Token    IRToken










irParseIter :: [IRParseItem] ->  [IRParseItem] -> Either IRErr IRParseItem
irParseIter [] [] = Left $ IRErr (IRPos 1 1 $ pack "????") $ pack "Nothing to parse?"

-- Numbers -- #N
irParseIter header ((PI_Token (NumToken p1 n1))
                   :(PI_Token (NumToken p0 n0)): stk)  = irParseIter header ((PI_NS       p0 [n1, n0]) : stk)

irParseIter header ((PI_Token (NumToken p1 n0))
                   :(PI_NS     p0 ns)          : stk)  = irParseIter header ((PI_NS       p0 (n0: ns)) : stk)

irParseIter header ((PI_Token (NumToken p0 n0)): stk)  = irParseIter header ((PI_NS       p0  [n0]   ) : stk)

-- Pointers -- *
irParseIter header ((PI_Token (PtrToken p1))
                   :(PI_Token (PtrToken p0))   : stk)  = irParseIter header ((PI_Ptr      p0  2      ) : stk)

irParseIter header ((PI_Token (PtrToken p1   ))
                   :(PI_Ptr    p0 n )          : stk)  = irParseIter header ((PI_Ptr      p0  (n + 1)) : stk)

irParseIter header ((PI_Ptr    p0 n)           : stk)  = irParseIter header ((PI_Ptr      p0  1      ) : stk)

-- Newlines -- \n
irParseIter header ((PI_Token (NewLine  p1))
                   :(PI_NL     p0  )           : stk)  = irParseIter header ((PI_NL       p0)          : stk)

irParseIter header ((PI_Token (NewLine  p1))
                   :(PI_Token (NewLine  p0))   : stk)  = irParseIter header ((PI_NL       p0)          : stk)

irParseIter header ((PI_Token (NewLine  p0))   : stk)  = irParseIter header ((PI_NL       p0)          : stk)

-- Arrays -- [N]
irParseIter header ((PI_Token (ArrToken p1 n1))
                   :(PI_Token (ArrToken p0 n0)): stk)  = irParseIter header ((PI_MultiArr p0 [n1, n0]) : stk)

irParseIter header ((PI_Token (ArrToken p1 n0))
                   :(PI_MultiArr p0 ns)        : stk)  = irParseIter header ((PI_MultiArr p0 (n0: ns)) : stk)

irParseIter header ((PI_Token (ArrToken p0 n0)): stk)  = irParseIter header ((PI_MultiArr p0  [n0]   ) : stk)

-- Nodes -- #N op #A #B ... \n
irParseIter header ((PI_NL    p3              )
                   :params
                   :(PI_Token (FuncToken p1 fn))
                   :(PI_Token (NumToken  p0 n0)): stk) = irParseIter header ((PI_Node p0 n0 fn params) : stk)











irParse :: [IRToken] -> Either IRErr IRParseItem
irParse tks = irParseIter [] $ L.map PI_Token tks
