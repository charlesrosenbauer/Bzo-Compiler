module BackendTypes where




{-
type BGId   = Int





data BGType =
  BGI8  | BGI16 | BGI32 | BGI64 |
  BGU8  | BGU16 | BGU32 | BGU64 |
  BGF32 | BGF64 | BGStr | BGBl
  | BGStruct BGId
  | BGUnion
  | BGArray  BGId Int
  | BGMap    BGType BGType
  | BGFunc   BGType BGType




data BGArithop =
  BGIADD | BGISUB | BGIMUL | BGIDIV | BGIMOD |
  BGFADD | BGFSUB | BGFMUL | BGFDIV | BGFMOD |
  BGBXOR | BGBOR  | BGBAND | BGBNOT | BGBSHL |
  BGBSHR | BGLOR  | BGLAND | BGLNOT | BGLSS  |
  BGGTR  | BGLSE  | BGGTE  | BGEQ   | BGNEQ  |
  BGEZ   | BGNZ





data BGExpr =
    BGArith BGArithop  BGExpr BGExpr
  | BGOther BGOtherop [BGExpr]
  | BGVar   BGId
  | BGCast  BGType BGExpr
  | BGVrSet [BGId] BGExpr
  | BGCall  BGId [BGExpr]
  | BGCase  [(Pattern, BGExpr)]
  | BGLmda  Pattern [BGType] [BGType] BGExpr






data Pattern =
    PatWild
  | PatInt  Integer
  | PatFlt  Double
  | PatStr  String
  | PatType BGId BGType
  | PatFunc BGId
  | PatTVar BGId BGType






data BGOtherop =
  BGDeref | BGRef   | BGArrayIx | BGReadIx | BGWriteIx |
  BGMpInst| BGMpRmov| BGMpGet   | BGMpAdjst| BGArGet   |
  BGArSet | BGMapOp | BGFold    | BGScan   | BGFilter  |
  BGSlice
-}


{-
  PLAN:
    Blocks will be generated as functions in the form : foo(in, out *interface{})bool

    Inputs and outputs are just empty interface pointers.

    Pattern matching is done in the block to try to extract the type from the interface.
    If this fails, the function returns false.
    If this succeeds, the function evaluates the actual block content, then returns true.

    Sum expressions : in (foo, bar) out
    ... are implemented by if-then-else-ing our way through a list of blocks.
    If one fails pattern matching, we try the next one.
    If all fail, then we just report an error. Try to keep the location of the
      error for debugging and for treating the programmer humanely.

    The benefits of this:
      * All functions have the same type signature
      * Complex logic turns into just guard expressions
      * The abstract mental model of Bzo, as well as the syntax, remain quite
          close to what is going on under the hood, simplifying the backend.



-}
--data BG_Block = BG_Block BG_Pattern [([Int], [Int], Expr)]
