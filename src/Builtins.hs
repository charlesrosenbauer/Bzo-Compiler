{-# LANGUAGE OverloadedStrings #-}

module Builtins where
import Data.Int
import qualified Data.Text as T










isBuiltinType :: T.Text -> Int64

--Integers
isBuiltinType "$Int8"   = 1
isBuiltinType "$Int16"  = 2
isBuiltinType "$Int32"  = 3
isBuiltinType "$Int64"  = 4
isBuiltinType "$Unt8"   = 5
isBuiltinType "$Unt16"  = 6
isBuiltinType "$Unt32"  = 7
isBuiltinType "$Unt64"  = 8

--Floating Point
isBuiltinType "$Flt16"  = 9
isBuiltinType "$Flt32"  = 10
isBuiltinType "$Flt64"  = 11

--Unums
isBuiltinType "$Unm16"  = 12
isBuiltinType "$Unm32"  = 13
isBuiltinType "$Unm64"  = 14

--Text
isBuiltinType "$ASCII"  = 15
isBuiltinType "$UTF8"   = 16
isBuiltinType "$UTF16"  = 17
isBuiltinType "$UTF32"  = 18

--Bools
isBuiltinType "$BoolTF" = 19

--Internal/Unsafe/FFI Types
isBuiltinType "$Ptr"    = 1001
isBuiltinType "$UntPtr" = 1002
isBuiltinType "$Unique" = 1003
isBuiltinType "$Effect" = 1004

isBuiltinType _         = 0










isBuiltinFunc :: T.Text -> Int64

--Arithmetic / Bitwise
isBuiltinFunc "$add-binop"    = 1
isBuiltinFunc "$sub-binop"    = 2
isBuiltinFunc "$mul-binop"    = 3
isBuiltinFunc "$div-binop"    = 4
isBuiltinFunc "$mod-binop"    = 5
isBuiltinFunc "$neg-op"       = 6
isBuiltinFunc "$and-binop"    = 7
isBuiltinFunc "$or-binop"     = 8
isBuiltinFunc "$xor-binop"    = 9
isBuiltinFunc "$not-op"       = 10
isBuiltinFunc "$rshift-binop" = 11
isBuiltinFunc "$lshift-binop" = 12
isBuiltinFunc "$ctlz-op"      = 13
isBuiltinFunc "$cttz-op"      = 14
isBuiltinFunc "$popcount-op"  = 15
isBuiltinFunc "$toBits"       = 16
isBuiltinFunc "$trunc"        = 17
isBuiltinFunc "$floatcast"    = 18
isBuiltinFunc "$unumcast"     = 19

--Comparison
isBuiltinFunc "$gtr-binop"    = 1001
isBuiltinFunc "$lss-binop"    = 1002
isBuiltinFunc "$geq-binop"    = 1003
isBuiltinFunc "$leq-binop"    = 1004
isBuiltinFunc "$eql-binop"    = 1005
isBuiltinFunc "$neq-binop"    = 1006

--Higher Order Functions
isBuiltinFunc "$map"          = 2001
isBuiltinFunc "$reduce"       = 2002
isBuiltinFunc "$fold"         = 2003
isBuiltinFunc "$scan"         = 2004
isBuiltinFunc "$chain"        = 2005
isBuiltinFunc "zip"           = 2006

--Array
isBuiltinFunc "$arrLength"    = 3001
isBuiltinFunc "$getIndex"     = 3002
isBuiltinFunc "$setIndex"     = 3003

--Math
isBuiltinFunc "$exp-binop"    = 4001
isBuiltinFunc "$surd-binop"   = 4002
isBuiltinFunc "$log-binop"    = 4003
isBuiltinFunc "$ln-op"        = 4004
isBuiltinFunc "$sine"         = 4005
isBuiltinFunc "$cosine"       = 4006
isBuiltinFunc "$tangent"      = 4007
isBuiltinFunc "$arcsine"      = 4008
isBuiltinFunc "$arccosine"    = 4009
isBuiltinFunc "$arctangent"   = 4010
isBuiltinFunc "$hsine"        = 4011
isBuiltinFunc "$hcosine"      = 4012
isBuiltinFunc "$htangent"     = 4013
isBuiltinFunc "$arc-hsine"    = 4014
isBuiltinFunc "$arc-hcosine"  = 4015
isBuiltinFunc "$arc-htangent" = 4016
isBuiltinFunc "$floor"        = 4017
isBuiltinFunc "$ciel"         = 4018
isBuiltinFunc "$round"        = 4019
isBuiltinFunc "$factorial"    = 4020
isBuiltinFunc "$gamma"        = 4021
isBuiltinFunc "$nCr"          = 4022
isBuiltinFunc "$nPr"          = 4023

isBuiltinFunc _               = 0
