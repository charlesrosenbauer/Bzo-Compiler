{-# LANGUAGE OverloadedStrings #-}

module Builtins where
import Data.Int
import qualified Data.Text as T
import qualified Data.Map.Strict as M










isBuiltinType :: T.Text -> Int64

--Integers
isBuiltinType "#Int8"   = 1
isBuiltinType "#Int16"  = 2
isBuiltinType "#Int32"  = 3
isBuiltinType "#Int64"  = 4
isBuiltinType "#Unt8"   = 5
isBuiltinType "#Unt16"  = 6
isBuiltinType "#Unt32"  = 7
isBuiltinType "#Unt64"  = 8

--Floating Point
isBuiltinType "#Flt16"  = 9
isBuiltinType "#Flt32"  = 10
isBuiltinType "#Flt64"  = 11

--Unums
isBuiltinType "#Unm16"  = 12
isBuiltinType "#Unm32"  = 13
isBuiltinType "#Unm64"  = 14

--Text
isBuiltinType "#ASCII"  = 15
isBuiltinType "#UTF8"   = 16
isBuiltinType "#UTF16"  = 17
isBuiltinType "#UTF32"  = 18

--Bools
isBuiltinType "#BoolTF" = 19

--Internal/Unsafe/FFI Types
isBuiltinType "#Ptr"    = 10001
isBuiltinType "#UntPtr" = 10002
isBuiltinType "#Unique" = 10003
isBuiltinType "#Effect" = 10004
isBuiltinType "#Any"    = 10005

isBuiltinType _         = 0


topBuiltinType :: Int
topBuiltinType = 20000










isBuiltinFunc :: T.Text -> Int64

--Arithmetic / Bitwise
isBuiltinFunc "#add-binop"    = 20001
isBuiltinFunc "#sub-binop"    = 20002
isBuiltinFunc "#mul-binop"    = 20003
isBuiltinFunc "#div-binop"    = 20004
isBuiltinFunc "#mod-binop"    = 20005
isBuiltinFunc "#neg-op"       = 20006
isBuiltinFunc "#and-binop"    = 20007
isBuiltinFunc "#or-binop"     = 20008
isBuiltinFunc "#xor-binop"    = 20009
isBuiltinFunc "#not-op"       = 20010
isBuiltinFunc "#rshift-binop" = 20011
isBuiltinFunc "#lshift-binop" = 20012
isBuiltinFunc "#ctlz-op"      = 20013
isBuiltinFunc "#cttz-op"      = 20014
isBuiltinFunc "#popcount-op"  = 20015
isBuiltinFunc "#toBits"       = 20016
isBuiltinFunc "#trunc"        = 20017
isBuiltinFunc "#floatcast"    = 20018
isBuiltinFunc "#unumcast"     = 20019

--Comparison
isBuiltinFunc "#gtr-binop"    = 30001
isBuiltinFunc "#lss-binop"    = 30002
isBuiltinFunc "#geq-binop"    = 30003
isBuiltinFunc "#leq-binop"    = 30004
isBuiltinFunc "#eql-binop"    = 30005
isBuiltinFunc "#neq-binop"    = 30006

--Higher Order Functions
isBuiltinFunc "#map"          = 40001
isBuiltinFunc "#reduce"       = 40002
isBuiltinFunc "#fold"         = 40003
isBuiltinFunc "#scan"         = 40004
isBuiltinFunc "#chain"        = 40005
isBuiltinFunc "zip"           = 40006

--Array
isBuiltinFunc "#arrLength"    = 50001
isBuiltinFunc "#getIndex"     = 50002
isBuiltinFunc "#setIndex"     = 50003

--Math
isBuiltinFunc "#exp-binop"    = 60001
isBuiltinFunc "#surd-binop"   = 60002
isBuiltinFunc "#log-binop"    = 60003
isBuiltinFunc "#ln-op"        = 60004
isBuiltinFunc "#sine"         = 60005
isBuiltinFunc "#cosine"       = 60006
isBuiltinFunc "#tangent"      = 60007
isBuiltinFunc "#arcsine"      = 60008
isBuiltinFunc "#arccosine"    = 60009
isBuiltinFunc "#arctangent"   = 60010
isBuiltinFunc "#hsine"        = 60011
isBuiltinFunc "#hcosine"      = 60012
isBuiltinFunc "#htangent"     = 60013
isBuiltinFunc "#arc-hsine"    = 60014
isBuiltinFunc "#arc-hcosine"  = 60015
isBuiltinFunc "#arc-htangent" = 60016
isBuiltinFunc "#floor"        = 60017
isBuiltinFunc "#ciel"         = 60018
isBuiltinFunc "#round"        = 60019
isBuiltinFunc "#factorial"    = 60020
isBuiltinFunc "#gamma"        = 60021
isBuiltinFunc "#nCr"          = 60022
isBuiltinFunc "#nPr"          = 60023

isBuiltinFunc _               = 0


topBuiltinFunc :: Int
topBuiltinFunc = 80000


topBuiltin :: Int
topBuiltin = 100000
