{
module BzoLexer (
  BzoToken(..),
  fileLexer
) where

import BzoTypes
}










%wrapper "basic"










$digit       = [0-9]
$lilalpha    = [a-z]
$bigalpha    = [A-Z]
$ls_ds       = [a-zA-Z0-9]
$nl          = [\n]
$special     = [\: \; \. \, \' \( \) \[ \] \{ \} \$ \@ \~ \_ $white \n \"]    --"
$symbol      = [\! \@ \# \% \^ \& \* \+ \- \= \\ \| \/ \? \< \> \`]
$commentable = $printable # \'
$charable    = $printable # [$special $white]
$lilcharable = $printable # [$special $white $bigalpha]
$stringable  = $printable # \"



-- "






tokens :-

  [$white # $nl]+                                           ;
  [\'] [$commentable]* [\']                                 ;
  \" [$stringable]* \"                                      { \s -> TkStr       ( BzoPos 0 0 "") (read s) }   --"
  $nl                                                       { \s -> TkNewline   $ BzoPos 0 0 ""}
  $digit+ [\.] $digit+                                      { \s -> TkFlt       ( BzoPos 0 0 "") (read s) }
  $digit+                                                   { \s -> TkInt       ( BzoPos 0 0 "") (read s) }
  $lilcharable+ $charable*                                  { \s -> TkId        ( BzoPos 0 0 "") s }
  $bigalpha+ [$lilalpha $symbol $digit \_]*                 { \s -> TkTypeId    ( BzoPos 0 0 "") s }
  \$ [$charable]+                                           { \s -> TkBuiltin   ( BzoPos 0 0 "") s }
  ":"                                                       { \s -> TkFilterSym $ BzoPos 0 0 ""}
  ";"                                                       { \s -> TkLambdaSym $ BzoPos 0 0 ""}
  "::"                                                      { \s -> TkDefine    $ BzoPos 0 0 ""}
  ".."                                                      { \s -> TkArrMod    $ BzoPos 0 0 ""}
  "()"                                                      { \s -> TkTupEmpt   $ BzoPos 0 0 ""}
  "[]"                                                      { \s -> TkArrGnrl   $ BzoPos 0 0 ""}
  "."                                                       { \s -> TkSepExpr   $ BzoPos 0 0 ""}
  ","                                                       { \s -> TkSepPoly   $ BzoPos 0 0 ""}
  "~"                                                       { \s -> TkMutable   $ BzoPos 0 0 ""}
  \@                                                        { \s -> TkReference $ BzoPos 0 0 ""}
  "_"                                                       { \s -> TkWildcard  $ BzoPos 0 0 ""}
  ";;"                                                      { \s -> TkFnSym     $ BzoPos 0 0 ""}
  "("                                                       { \s -> TkStartTup  $ BzoPos 0 0 ""}
  ")"                                                       { \s -> TkEndTup    $ BzoPos 0 0 ""}
  "["                                                       { \s -> TkStartDat  $ BzoPos 0 0 ""}
  "]"                                                       { \s -> TkEndDat    $ BzoPos 0 0 ""}
  "{"                                                       { \s -> TkStartDo   $ BzoPos 0 0 ""}
  "}"                                                       { \s -> TkEndDo     $ BzoPos 0 0 ""}



{

fileLexer :: String -> [BzoToken]
fileLexer = alexScanTokens

}
