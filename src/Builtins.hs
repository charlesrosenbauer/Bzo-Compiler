{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2020 Charles Rosenbauer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.-}

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

--Primitives
isBuiltinType "#Int"    = 12
isBuiltinType "#Flt"    = 13
isBuiltinType "#True"   = 14
isBuiltinType "#False"  = 15
isBuiltinType "#Bool"   = 16

--Text
isBuiltinType "#Str"    = 17
isBuiltinType "#Char"   = 18


--Regular Expressions
isBuiltinType "#Regex"  = 28

--Internal/Unsafe/FFI Types
isBuiltinType "#Ptr"    = 10001
isBuiltinType "#UntPtr" = 10002
isBuiltinType "#Unique" = 10003
isBuiltinType "#Effect" = 10004
isBuiltinType "#Any"    = 10005
isBuiltinType "#GenArr" = 10006

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
isBuiltinFunc "#positcast"    = 20019
isBuiltinFunc "#shadeInsert"  = 20020
isBuiltinFunc "#shadeRemove"  = 20021
isBuiltinFunc "#shadeRead"    = 20022
isBuiltinFunc "#shadeComp"    = 20023
isBuiltinFunc "#shadeUnion"   = 20024
isBuiltinFunc "#shadeIntersection" = 20025
isBuiltinFunc "#shadeDifference"   = 20026
isBuiltinFunc "#shadeInverse"      = 20027
isBuiltinFunc "#rrotate-binop"= 20028
isBuiltinFunc "#lrotate-binop"= 20029

--Comparison
isBuiltinFunc "#gtr-binop"    = 30001
isBuiltinFunc "#lss-binop"    = 30002
isBuiltinFunc "#geq-binop"    = 30003
isBuiltinFunc "#leq-binop"    = 30004
isBuiltinFunc "#eql-binop"    = 30005
isBuiltinFunc "#neq-binop"    = 30006

--Higher Order Functions
isBuiltinFunc "#map"          = 40001
isBuiltinFunc "#pfold"        = 40002
isBuiltinFunc "#sfold"        = 40003
isBuiltinFunc "#pscan"        = 40004
isBuiltinFunc "#sscan"        = 40005
isBuiltinFunc "#zip"          = 40006
isBuiltinFunc "#unzip"        = 40007
isBuiltinFunc "#toMap"        = 40008
isBuiltinFunc "#toArr"        = 40009
isBuiltinFunc "#toSet"        = 40010
isBuiltinFunc "#reverse"      = 40011
isBuiltinFunc "#take"         = 40012
isBuiltinFunc "#drop"         = 40013
isBuiltinFunc "#intersperse"  = 40014
isBuiltinFunc "#sort"         = 40015
isBuiltinFunc "#sortBy"       = 40016
isBuiltinFunc "#nub"          = 40017
isBuiltinFunc "#nubBy"        = 40018
isBuiltinFunc "#subcycle"     = 40019
isBuiltinFunc "#head"         = 40020
isBuiltinFunc "#tail"         = 40021
isBuiltinFunc "#filter"       = 40022
isBuiltinFunc "#concat"       = 40023
isBuiltinFunc "#concatMap"    = 40024
isBuiltinFunc "#rotate"       = 40025
isBuiltinFunc "#square"       = 40026
isBuiltinFunc "#cube"         = 40027
isBuiltinFunc "#tesseract"    = 40028
isBuiltinFunc "#any"          = 40029
isBuiltinFunc "#all"          = 40030
isBuiltinFunc "#none"         = 40031
isBuiltinFunc "#iter"         = 40032
isBuiltinFunc "#project"      = 40033
isBuiltinFunc "#ife"          = 40034
isBuiltinFunc "#guard"        = 40035
isBuiltinFunc "#isPrefix"     = 40036
isBuiltinFunc "#isSuffix"     = 40037
isBuiltinFunc "#arrLength"    = 40038
isBuiltinFunc "#getIndex"     = 40039
isBuiltinFunc "#setIndex"     = 40040
isBuiltinFunc "#cons"         = 40041
isBuiltinFunc "#zipWith"      = 40042
isBuiltinFunc "#replace"      = 40043
isBuiltinFunc "#adjust"       = 40044

--String
isBuiltinFunc "#to-uppercase" = 50001
isBuiltinFunc "#to-lowercase" = 50002
isBuiltinFunc "#str-compare"  = 50003
isBuiltinFunc "#compileRegex" = 50004
isBuiltinFunc "#matchRegex"   = 50005

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
isBuiltinFunc "#sqrt"         = 60024
isBuiltinFunc "#cbrt"         = 60025
isBuiltinFunc "#log2"         = 60026
isBuiltinFunc "#log10"        = 60027
isBuiltinFunc "#exp-unop"     = 60028

--Effect / FFI / Unsafe
isBuiltinFunc "#print"        = 70001
isBuiltinFunc "#printf"       = 70002
isBuiltinFunc "#ffi"          = 70003
isBuiltinFunc "#coerce"       = 70004
isBuiltinFunc "#fail!"        = 70005
isBuiltinFunc "#errLog"       = 70006

--Map Functions
isBuiltinFunc "#insertMap"    = 80001
isBuiltinFunc "#removeMap"    = 80002
isBuiltinFunc "#adjustMap"    = 80003
isBuiltinFunc "#assocsMap"    = 80004
isBuiltinFunc "#createMap"    = 80005
isBuiltinFunc "#memberMap"    = 80006
isBuiltinFunc "#lookupMap"    = 80007

isBuiltinFunc _               = 0


topBuiltinFunc :: Int
topBuiltinFunc = 80000


topBuiltin :: Int
topBuiltin = 100000










-- Constants for a bunch of primitive types
t_i8   = 1
t_i16  = 2
t_i32  = 3
t_i64  = 4
t_u8   = 5
t_u16  = 6
t_u32  = 7
t_u64  = 8
t_f16  = 9
t_f32  = 10
t_f64  = 11
t_str  = 16
