module Codegen where
import Data.Int
import Data.Map










type BzoExprAST = Map Int64 BzoASTNode










data LLVMType
      = SInt64 | SInt32 | SInt16 | SInt8
      | UInt64 | Uint32 | Uint16 | UInt8
      | Flt64  | Flt32  | Flt16  | Bl
      | UTF8   | ASCII  | Array Int64 LLVMType
      | Cmpd [LLVMType] | Ptr LLVMType










data BzoASTNode
      = ADD  LLVMType Int64 Int64
      | SUB  LLVMType Int64 Int64
      | MUL  LLVMType Int64 Int64
      | DIV  LLVMType Int64 Int64
      | MOD  LLVMType Int64 Int64
      | AND  LLVMType Int64 Int64
      | OR   LLVMType Int64 Int64
      | XOR  LLVMType Int64 Int64
      | SIGN LLVMType Int64
      | NOT  LLVMType Int64
      | GET  LLVMType Int64 Int64           -- GetElementPtr
      | FIX  LLVMType LLVMType Int64        -- Cast from Float to Int
      | FLT  LLVMType LLVMType Int64        -- Cast from Int to Float
