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
  BGBSHR | BGLOR  | BGLAND | BGLNOT | BGBNIL |
  BGBVAR

data BGOps =
    BGARith BGArithop BGOps BGOps
  | BGVrSet BGId BGOps
  | BGCall  BGId [BGOps]
