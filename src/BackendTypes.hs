module BackendTypes where





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
    BGArith BGArithop BGExpr BGExpr
  | BGVar   BGId
  | BGCast  BGType BGExpr
  | BGVrSet [BGId] BGExpr
  | BGCall  BGId [BGExpr]






data OtherOp =
  BGDeref | BGRef   | BGArrayIx | BGReadIx | BGWriteIx |
  BGInsert| BGRemove| BGMapOp   | BGFold   | BGScan    |
  BGFilter| BGSlice
