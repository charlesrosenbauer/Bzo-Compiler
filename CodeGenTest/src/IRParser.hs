module IRParser where
import IRLexer
import Data.List
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
  | PI_NS       IRPos [IRParseItem]
  | PI_Nodes    IRPos [IRParseItem]
  | PI_Def      IRPos IRParseItem
  | PI_Defs     IRPos [IRParseItem]
  | PI_Const    IRPos Text
  | PI_ConstInt IRPos Text Int
  | PI_ConstStr IRPos Text Text
  | PI_Str      IRPos Text
  | PI_Int      IRPos Int
  | PI_Token    IRToken
