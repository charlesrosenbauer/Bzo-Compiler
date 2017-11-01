{-# LANGUAGE OverloadedStrings #-}

module Builtins where
import qualified Data.Text as T










isBuiltinType :: T.Text -> Bool

--Integers
isBuiltinType "$Int8"   = True
isBuiltinType "$Int16"  = True
isBuiltinType "$Int32"  = True
isBuiltinType "$Int64"  = True
isBuiltinType "$Unt8"   = True
isBuiltinType "$Unt16"  = True
isBuiltinType "$Unt32"  = True
isBuiltinType "$Unt64"  = True

--Floating Point
isBuiltinType "$Flt16"  = True
isBuiltinType "$Flt32"  = True
isBuiltinType "$Flt64"  = True

--Unums
isBuiltinType "$Unm16"  = True
isBuiltinType "$Unm32"  = True
isBuiltinType "$Unm64"  = True

--Text
isBuiltinType "$ASCII"  = True
isBuiltinType "$UTF8"   = True
isBuiltinType "$UTF16"  = True
isBuiltinType "$UTF32"  = True

--Bools
isBuiltinType "$BoolTF" = True

--Internal/Unsafe/FFI Types
isBuiltinType "$Ptr"    = True
isBuiltinType "$UntPtr" = True
isBuiltinType "$Unique" = True
isBuiltinType "$Effect" = True

isBuiltinType _         = False










isBuiltinFunc :: T.Text -> Bool

--Arithmetic / Bitwise
isBuiltinFunc "$add-binop"    = True
isBuiltinFunc "$sub-binop"    = True
isBuiltinFunc "$mul-binop"    = True
isBuiltinFunc "$div-binop"    = True
isBuiltinFunc "$mod-binop"    = True
isBuiltinFunc "$neg-op"       = True
isBuiltinFunc "$and-binop"    = True
isBuiltinFunc "$or-binop"     = True
isBuiltinFunc "$xor-binop"    = True
isBuiltinFunc "$not-op"       = True
isBuiltinFunc "$rshift-binop" = True
isBuiltinFunc "$lshift-binop" = True
isBuiltinFunc "$ctlz-op"      = True
isBuiltinFunc "$cttz-op"      = True
isBuiltinFunc "$popcount-op"  = True
isBuiltinFunc "$toBits"       = True
isBuiltinFunc "$trunc"        = True
isBuiltinFunc "$floatcast"    = True
isBuiltinFunc "$unumcast"     = True

--Comparison
isBuiltinFunc "$gtr-binop"    = True
isBuiltinFunc "$lss-binop"    = True
isBuiltinFunc "$geq-binop"    = True
isBuiltinFunc "$leq-binop"    = True
isBuiltinFunc "$eql-binop"    = True
isBuiltinFunc "$neq-binop"    = True

--Higher Order Functions
isBuiltinFunc "$map"          = True
isBuiltinFunc "$reduce"       = True
isBuiltinFunc "$fold"         = True
isBuiltinFunc "$scan"         = True
isBuiltinFunc "$chain"        = True
isBuiltinFunc "zip"           = True

--Array
isBuiltinFunc "$arrLength"    = True
isBuiltinFunc "$getIndex"     = True
isBuiltinFunc "$setIndex"     = True

--Math
isBuiltinFunc "$exp-binop"    = True
isBuiltinFunc "$surd-binop"   = True
isBuiltinFunc "$log-binop"    = True
isBuiltinFunc "$ln-op"        = True
isBuiltinFunc "$sine"         = True
isBuiltinFunc "$cosine"       = True
isBuiltinFunc "$tangent"      = True
isBuiltinFunc "$arcsine"      = True
isBuiltinFunc "$arccosine"    = True
isBuiltinFunc "$arctangent"   = True
isBuiltinFunc "$hsine"        = True
isBuiltinFunc "$hcosine"      = True
isBuiltinFunc "$htangent"     = True
isBuiltinFunc "$arc-hsine"    = True
isBuiltinFunc "$arc-hcosine"  = True
isBuiltinFunc "$arc-htangent" = True
isBuiltinFunc "$floor"        = True
isBuiltinFunc "$ciel"         = True
isBuiltinFunc "$round"        = True
isBuiltinFunc "$factorial"    = True
isBuiltinFunc "$gamma"        = True
isBuiltinFunc "$nCr"          = True
isBuiltinFunc "$nPr"          = True

isBuiltinFunc _               = False
