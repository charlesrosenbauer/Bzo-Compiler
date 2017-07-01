module BzoReducedAST where
import BzoTypes

{-
  This is a simplified AST data structure.
  It's designed to be easily convertable to code (LLVM / ASM)
  Ideally, it should include both simple and complex operations, as this
  makes it easier to take advantage of complex operations in architectures
  that support them. As for architectures that do not support them, they
  may be replaced with software implementations.

  It also allows complex, implementation-specific functions like Map and Reduce
  to be easier for the compiler to recognize.
-}










data OpPrecision
  = BYTE
  | HALF
  | WORD
  | WRDX    -- | Some major architectures support 80 bit Floating Point
  | DWRD










data BzoAST
  = ASTFunc [BzoAST],
  -- | Integer Operations
  | ASTIAdd BzoAST BzoAST OpPrecision,  -- | Int Addition
  | ASTISub BzoAST BzoAST OpPrecision,  -- | Int Subtraction
  | ASTIMul BzoAST BzoAST OpPrecision,  -- | Int Multiplication
  | ASTIDiv BzoAST BzoAST OpPrecision,  -- | Int Division
  | ASTIMod BzoAST BzoAST OpPrecision,  -- | Int Modulus
  | ASTICmp BzoAST BzoAST OpPrecision,  -- | Int Compare
  -- | Floating Point Operations
  | ASTFAdd BzoAST BzoAST OpPrecision,  -- | Flt Addition
  | ASTFSub BzoAST BzoAST OpPrecision,  -- | Flt Subtraction
  | ASTFMul BzoAST BzoAST OpPrecision,  -- | Flt Multiplication
  | ASTFDiv BzoAST BzoAST OpPrecision,  -- | Flt Division
  | ASTFMod BzoAST BzoAST OpPrecision,  -- | Flt Modulus
  | ASTFCmp BzoAST BzoAST OpPrecision,  -- | Flt Compare
  | ASTFSin BzoAST        OpPrecision,  -- | Flt Sine
  | ASTFCos BzoAST        OpPrecision,  -- | Flt Cosine
  | ASTFTan BzoAST        OpPrecision,  -- | Flt Tangent
  | ASTFASn BzoAST        OpPrecision,  -- | Flt Arc sine
  | ASTFACs BzoAST        OpPrecision,  -- | Flt Arc cosine
  | ASTFATn BzoAST        OpPrecision,  -- | Flt Arc tangent
  | ASTFExp BzoAST BzoAST OpPrecision,  -- | Flt Exponent
  | ASTFLg2 BzoAST        OpPrecision,  -- | Flt Log 2
  | ASTFLgE BzoAST        OpPrecision,  -- | Flt Log e
  | ASTFL10 BzoAST        OpPrecision,  -- | Flt Log 10
  | ASTFLog BzoAST BzoAST OpPrecision,  -- | Flt Logarithm
  | ASTFSqt BzoAST        OpPrecision,  -- | Flt Square Root
  | ASTFCbt BzoAST        OpPrecision,  -- | Flt Cube Root
  | ASTFRtN BzoAST BzoASt OpPrecision,  -- | Flt Nth Root
  -- | Bitwise Operations
  | ASTAND  BzoAST BzoAST OpPrecision,  -- | Bitwise And
  | ASTOR   BzoAST BzoAST OpPrecision,  -- | Bitwise Inclusive Or
  | ASTXOR  BzoAST BzoAST OpPrecision,  -- | Bitwise Exclusive Or
  | ASTNOT  BzoAST        OpPrecision,  -- | Bitwise Not
  | ASTLSR  BzoAST BzoAST OpPrecision,  -- | Bitwise Shift Right
  | ASTLSR  BzoAST BzoAST OpPrecision,  -- | Bitwise Shift Left
  | ASTPCNT BzoAST        OpPrecision,  -- | Popcount / Hamming Weight
  | ASTCTLZ BzoAST        OpPrecision,  -- | Count Leading Zeroes
  | ASTCTTZ BzoAST        OpPrecision,  -- | Count Trailing Zeroes
  -- | Control Functions
  | ASTCall BzoAST BzoAST,              -- | Function Call
  | ASTTask BzoAST BzoAST,              -- |
  | ASTJmRt BzoAST BzoAST,              -- | Jump to Function, then return to current position
  | ASTJump BzoAST BzoAST,              -- | Unconditional Jump
  | ASTJIF  BzoAST       ,              -- | Jump If
  | ASTRtrn ,                           -- | Return
  | ASTVSet BzoAST BzoAST,              -- | Set Variable
  | ASTVGet BzoAST BzoAST,              -- | Get Variable
  | ASTDest BzoAST       ,              -- | Delete Memory
  | ASTAllc BzoAST       ,              -- | Allocate Memory
  | ASTDRef BzoAST       ,              -- | Dereference Value
  | ASTVRef BzoAST       ,              -- | Reference Value
  | ASTPADD BzoAST BzoAST,              -- | Add to Pointer
  | ASTGArr BzoAST BzoAST,              -- | Get data from Array
  | ASTSArr BzoAST BzoAST,              -- | Set data in Array
  -- | High Order Functions
  | ASTMap  BzoAST BzoAST,              -- | Map Fn to Array
  | ASTPRdc BzoAST BzoAST,              -- | Parallel Reduce Array with Fn
  | ASTSRdc BzoAST BzoAST,              -- | Sequential Reduce Array with Fn
  | ASTPScn BzoAST BzoAST,              -- | Parallel Scan Array with Fn
  | ASTSScn BzoAST BzoAST,              -- | Sequential Scan Array with Fn
  | ASTItrI BzoAST BzoAST Int,          -- | Informed Iterate Function on Data n times
  -- | Constant Values
  | ASTCmpd [BzoAST]                    -- | Compound Tuple
  | ASTPoly [BzoAST]                    -- | Polymorphic Tuple
  | ASTInt  Integer,                    -- | Integer
  | ASTFlt  Float,                      -- | Float
  | ASTStr  String,                     -- | String
  | ASTVar  Int                         -- | Variable <ID#>
