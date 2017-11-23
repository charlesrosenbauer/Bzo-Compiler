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
  | PI_Node     IRPos Int Text [IRParseItem]
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










irParseIter :: [IRParseItem] -> Either IRErr IRParseItem
irParseIter [] = Left $ IRErr (IRPos 1 1 $ pack "????") $ pack "Nothing to parse?"

irParseIter ((PI_Token (NumToken p0 n0))
            :(PI_Token (NumToken p1 n1)): ts) = irParseIter ((PI_NS    p0 [n1, n0]) : ts)

irParseIter ((PI_NS     p0 ns)
            :(PI_Token (NumToken p1 n0)): ts) = irParseIter ((PI_NS    p0 (n0: ns)) : ts)

irParseIter ((PI_Token (NumToken p0 n0)): ts) = irParseIter ((PI_NS    p0  [n0]   ) : ts)

irParseIter ((PI_Token (PtrToken p0))
            :(PI_Token (PtrToken p1))   : ts) = irParseIter ((PI_Ptr   p0  2      ) : ts)

irParseIter ((PI_Ptr    p0 n )
            :(PI_Token (PtrToken p1   )): ts) = irParseIter ((PI_Ptr   p0  (n + 1)) : ts)

irParseIter ((PI_Ptr    p0 n)           : ts) = irParseIter ((PI_Ptr   p0  1      ) : ts)









irParse :: [IRToken] -> Either IRErr IRParseItem
irParse tks = irParseIter $ L.map PI_Token tks
