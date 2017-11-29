module IRModel where
import Data.Text
import Data.List
import Data.Map.Strict










data FuncData = FuncType{
  inputTypes  :: [TypeData],
  outputTypes :: [TypeData],
  attributes  :: [Attribute],
  funcNodes   :: [Node],
  funcId      :: FnId }

type FnId = Int










data Node
  = CastNode   Int TypeData Int
  | BinopNode  Int Int Int
  | OpNode     Int Int
  | OutNode    Int TypeData
  | InNode     Int TypeData
  | RetNode    Int Int Int










data TypeData = TypeData [([Int], TyId)]

type TyId = Int










data Attribute
  = IntAttribute AttrId Int
  | StrAttribute AttrId Text
  | BlAttribute  AttrId

type AttrId = Int
