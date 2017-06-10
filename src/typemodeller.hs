module BzoTypeModeller where
import BzoSyntax
import BzoTypes
import HigherOrder










data FuncModel
  = FuncExpr {
      funcName  :: String }
  | FuncAlias {
      funcName  :: String,
      funcAlias :: String }
  | FuncConstant {
      funcName  :: String }
  | FuncGraph {
      funcName  :: String }










data TypeModel
  = TypeExpr {
      typeName  :: String }
  | TypeAlias {
      typeName  :: String,
      typeAlias :: String }
  | TypeContainer {
      typeName  :: String }
  | TypeStruct {
      typeName  :: String }
