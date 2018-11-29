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

--Posits
isBuiltinType "#Pst8"   = 12    -- I'm not sure if this will make it
isBuiltinType "#Pst16"  = 13
isBuiltinType "#Pst32"  = 14
isBuiltinType "#Pst64"  = 15

--Text
isBuiltinType "#ASCII"  = 16
isBuiltinType "#UTF8"   = 17
isBuiltinType "#UTF16"  = 18
isBuiltinType "#UTF32"  = 19

--Bools
isBuiltinType "#BoolTF" = 20

--Slides
isBuiltinType "#S8"     = 21
isBuiltinType "#S16"    = 22
isBuiltinType "#S32"    = 23
isBuiltinType "#S64"    = 24
isBuiltinType "#S128"   = 25
isBuiltinType "#S256"   = 26
isBuiltinType "#S512"   = 27

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
isBuiltinFunc "#positcast"    = 20019
isBuiltinFunc "#slideInsert"  = 20020
isBuiltinFunc "#slideRemove"  = 20021
isBuiltinFunc "#slideRead"    = 20022
isBuiltinFunc "#slideComp"    = 20023
isBuiltinFunc "#slideUnion"   = 20024
isBuiltinFunc "#slideIntersection" = 20025
isBuiltinFunc "#slideDifference"   = 20026
isBuiltinFunc "#slideInverse"      = 20027

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
