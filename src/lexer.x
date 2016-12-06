{
module BzoLexer (
  BzoToken(..),
  fileLexer
) where

import BzoTypes
}










%wrapper "basic"










$digit = [0-9]
$lilalpha = [a-z]
$bigalpha = [A-Z]
$alpha = [a-zA-z]
$nl = [\n]
$symbol = [\! \@ \# \% \^ \& \* \+ \- \= \\ \| \/ \? \< \> \`]
$commentable = $printable # \'
$stringable = $printable # \"



-- "






tokens :-

  $white+                                                 ;
  \' $commentable \'                                      ;
  '"' $stringable '"'                                     { \s -> TkStr       ( BzoPos 0 0 "")  s }
  $nl                                                     { \s -> TkNewline   $ BzoPos 0 0 ""}
  $digit+ \. $digit+                                      { \s -> TkFlt       ( BzoPos 0 0 "") (read s) }
  $digit+                                                 { \s -> TkInt       ( BzoPos 0 0 "") (read s) }
  [$lilalpha $symbol $digit]+[$alpha $symbol $digit \_ ]* { \s -> TkId        ( BzoPos 0 0 "") (read s) }
  [$bigalpha]+[$alpha $symbol $digit \_]*                 { \s -> TkTypeId    ( BzoPos 0 0 "") (read s) }
  \$ [$alpha $symbol $digit \_]+                          { \s -> TkBuiltin   ( BzoPos 0 0 "") (read s) }
  ":"                                                     { \s -> TkFilterSym $ BzoPos 0 0 ""}
  ";"                                                     { \s -> TkLambdaSym $ BzoPos 0 0 ""}
  "::"                                                    { \s -> TkDefine    $ BzoPos 0 0 ""}
  ".."                                                    { \s -> TkArrGnrl   $ BzoPos 0 0 ""}
  "()"                                                    { \s -> TkTupEmpt   $ BzoPos 0 0 ""}
  "[]"                                                    { \s -> TkArrGnrl   $ BzoPos 0 0 ""}
  "."                                                     { \s -> TkSepExpr   $ BzoPos 0 0 ""}
  ","                                                     { \s -> TkSepPoly   $ BzoPos 0 0 ""}
  "~"                                                     { \s -> TkMutable   $ BzoPos 0 0 ""}
  "@"                                                     { \s -> TkReference $ BzoPos 0 0 ""}
  "_"                                                     { \s -> TkWildcard  $ BzoPos 0 0 ""}
  ";;"                                                    { \s -> TkFnSym     $ BzoPos 0 0 ""}
  "("                                                     { \s -> TkStartTup  $ BzoPos 0 0 ""}
  ")"                                                     { \s -> TkEndTup    $ BzoPos 0 0 ""}
  "["                                                     { \s -> TkStartDat  $ BzoPos 0 0 ""}
  "]"                                                     { \s -> TkEndDat    $ BzoPos 0 0 ""}
  "{"                                                     { \s -> TkStartDo   $ BzoPos 0 0 ""}
  "}"                                                     { \s -> TkEndDo     $ BzoPos 0 0 ""}



{

fileLexer :: String -> [BzoToken]
fileLexer = alexScanTokens

}
