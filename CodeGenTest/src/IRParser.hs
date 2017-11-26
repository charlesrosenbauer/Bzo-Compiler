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
  | PI_ConstStr IRPos Text Text
  | PI_Str      IRPos Text
  | PI_Int      IRPos Int
  | PI_Token    IRToken
  deriving (Eq, Show)










irParseIter :: String -> [IRParseItem] ->  [IRParseItem] -> Either IRErr IRParseItem
irParseIter fname [] [] = Left $ IRErr (IRPos 1 1 $ pack fname) $ pack "Nothing to parse?"

-- Numbers -- #N
irParseIter fname tokens ((PI_Token (NumToken p1 n1))
                   :(PI_Token (NumToken p0 n0))       : stk)  = irParseIter fname tokens ((PI_NS       p0 [n1, n0]) : stk)

irParseIter fname tokens ((PI_Token (NumToken p1 n0))
                   :(PI_NS     p0 ns)                 : stk)  = irParseIter fname tokens ((PI_NS       p0 (n0: ns)) : stk)

irParseIter fname tokens ((PI_Token (NumToken p0 n0)) : stk)  = irParseIter fname tokens ((PI_NS       p0  [n0]   ) : stk)

-- Pointers -- *
irParseIter fname tokens ((PI_Token (PtrToken p1))
                   :(PI_Token (PtrToken p0))          : stk)  = irParseIter fname tokens ((PI_Ptr      p0  2      ) : stk)

irParseIter fname tokens ((PI_Token (PtrToken p1   ))
                   :(PI_Ptr    p0 n )                 : stk)  = irParseIter fname tokens ((PI_Ptr      p0  (n + 1)) : stk)

irParseIter fname tokens ((PI_Ptr    p0 n)            : stk)  = irParseIter fname tokens ((PI_Ptr      p0  1      ) : stk)

-- Newlines -- \n
irParseIter fname tokens ((PI_Token (NewLine  p1))
                   :(PI_NL     p0  )                  : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

irParseIter fname tokens ((PI_Token (NewLine  p1))
                   :(PI_Token (NewLine  p0))          : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

irParseIter fname tokens ((PI_Token (NewLine  p0))    : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

-- Arrays -- [N]
irParseIter fname tokens ((PI_Token (ArrToken p1 n1))
                   :(PI_Token (ArrToken p0 n0)):        stk)  = irParseIter fname tokens ((PI_MultiArr p0 [n1, n0]) : stk)

irParseIter fname tokens ((PI_Token (ArrToken p1 n0))
                   :(PI_MultiArr p0 ns)        :        stk)  = irParseIter fname tokens ((PI_MultiArr p0 (n0: ns)) : stk)

irParseIter fname tokens ((PI_Token (ArrToken p0 n0)):  stk)  = irParseIter fname tokens ((PI_MultiArr p0  [n0]   ) : stk)

-- Nodes -- #N op #A #B ... \n
irParseIter fname tokens ((PI_NL    p3              )
                   :params
                   :(PI_Token (FuncToken p1 fn))
                   :(PI_Token (NumToken  p0 n0)):       stk) = irParseIter fname tokens ((PI_Node p0 n0 fn params) : stk)

-- Types -- Ty | *Ty | [n]Ty | ...
irParseIter fname tokens ((PI_Token (TypeToken p2 ty))
                   :(PI_Ptr      p1 pn         )
                   :(PI_MultiArr p0 as         )      : stk) = irParseIter fname tokens ((PI_Type p0 as pn ty)     : stk)

irParseIter fname tokens ((PI_Token (TypeToken p1 ty))
                   :(PI_Ptr   p0 pn            )      : stk) = irParseIter fname tokens ((PI_Type p0 [] pn ty)     : stk)

irParseIter fname tokens ((PI_Token (TypeToken p1 ty))
                   :(PI_MultiArr p0 as         )      : stk) = irParseIter fname tokens ((PI_Type p0 as 0 ty)      : stk)

irParseIter fname tokens ((PI_Token (TypeToken p0 ty)): stk) = irParseIter fname tokens ((PI_Type p0 [] 0  ty)     : stk)

-- Fn Header
irParseIter fname tokens ((PI_NL p5)
                   :(PI_Token (OpenBrace p4))
                   :(PI_Token (NumToken  p3 n1))
                   :(PI_Token (NumToken  p2 n0))
                   :(PI_Token (FuncToken p1 fnid))
                   :(PI_Token (DefFunc   p0))         : stk) = irParseIter fname tokens ((PI_FnHeader p0 fnid n0 n1):stk)

-- Function Definition
irParseIter fname tokens ((PI_NL p3)
                   :(PI_Token (CloseBrace p2))
                   :(PI_Nodes p1 ns)
                   :(PI_FnHeader p0 fnid n0 n1)       : stk) = irParseIter fname tokens ((PI_FnDef p0 fnid n0 n1 ns):stk)

-- Ty Header
irParseIter fname tokens ((PI_NL p4)
                   :(PI_Token (OpenBrace p3))
                   :(PI_Token (NumToken  p2 n))
                   :(PI_Token (FuncToken p1 tyid))
                   :(PI_Token (DefType   p0))         : stk) = irParseIter fname tokens ((PI_TyHeader p0 tyid n)    :stk)

-- Type Definition
irParseIter fname tokens ((PI_NL p3)
                   :(PI_Token (CloseBrace p2))
                   :(PI_Nodes p1 ns)
                   :(PI_TyHeader p0 tyid n)           : stk) = irParseIter fname tokens ((PI_TyDef p0 tyid n ns)    :stk)

-- No rules work?
irParseIter fname [] [item] = Right item

irParseIter fname [] stack = Left $ IRErr (IRPos 1 1 $ pack fname) $ pack "Parser could not consume entire file."

irParseIter fname (t:tokens) stack = irParseIter fname tokens (t:stack)











irParse :: String -> [IRToken] -> Either IRErr IRParseItem
irParse fname tks = irParseIter fname [] $ L.map PI_Token tks
