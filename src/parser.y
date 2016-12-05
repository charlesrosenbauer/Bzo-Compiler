
{
module BzoParser where
import BzoTypes
import BzoSyntax
import BzoTokens
}










%name bzoParser
%tokentype { BzoToken }
%error { parseError }










%token
    '('           { TkStartTup  _ }
    ')'           { TkEndTup    _ }
    '['           { TkStartDat  _ }
    ']'           { TkEndDat    _ }
    '{'           { TkStartDo   _ }
    '}'           { TkEndDo     _ }
    '.'           { TkSepExpr   _ }
    ','           { TkSepPoly   _ }
    ':'           { TkFilterSym _ }
    ';'           { TkLambdaSym _ }
    '~'           { TkMutable   _ }
    '@'           { TkReference _ }
    '_'           { TkWildcard  _ }
    DEF           { TkDefine    _ }
    ';;'          { TkFnSym     _ }
    '()'          { TkTupEmpt   _ }
    '[]'          { TkArrGnrl   _ }
    '..'          { TkArrMod    _ }
    '\n'          { TkNewline   _ }
    TyId          { TkTypeId    _ $$ }
    Id            { TkId        _ $$ }
    INT           { TkInt       _ $$ }
    FLT           { TkFlt       _ $$ }
    STR           { TkStr       _ $$ }









%%

calls       : fnCall                                    { Calls [$1] }
            | line                                      { Calls [$1] }
            | lines                                     { Calls [$1] }
            | nl                                        { Calls [] }
            | calls calls                               { Calls ((calls $1) ++ (calls $2)) }

line        : statements nl                             { $1 }
            | statement  nl                             { $1 }
            | statement  expr nl                        { Statements ((exprs $1) ++ [$2]) }
            | statements expr nl                        { Statements ((exprs $1) ++ [$2]) }
            | expr       nl                             { Statements [$1] }

lines       : line  line                                { Statements ((exprs $1) ++ (exprs $2)) }
            | lines line                                { Statements ((exprs $1) ++ (exprs $2)) }

statements  : statement  statement                      { Statements ((exprs $1) ++ (exprs $2)) }
            | statements statement                      { Statements ((exprs $1) ++ (exprs $2)) }

statement   : expr '.'                                  { Statements [$1] }

expr        : expr expr                                 { Expr ((exprs $1) ++ (exprs $2)) }
            | TyId                                      { Expr [Constructor $1 $ DtUnspecified ModUnspecified] }
            | tuple block tuple                         { Expr [StatementBlock $1 $3 (exprs $2)] }
            | tuple                                     { Expr [$1] }
            | INT                                       { Expr [Atoms (AtmInt $1)] }
            | FLT                                       { Expr [Atoms (AtmFlt $1)] }
            | STR                                       { Expr [Atoms (AtmStr $1)] }
            | Id                                        { Expr [Atoms (AtmId  $1)] }
            | Id '..'                                   { Expr [ArrAtom (AtmId $1)] }
            | '_'                                       { Expr [Wildcard] }
            | lambda block                              { Expr [Lambda (pars $1) $2] }
            | lambda line                               { Expr [Lambda (pars $1) $2] }

tuple       : stup line  etup                           { TupleExpr (exprs $1) }
            | stup lines etup                           { TupleExpr (exprs $1) }
            | stup expr etup                            { TupleExpr [$2] }
            | stup statement  etup                      { TupleExpr [$2] }
            | stup statements etup                      { TupleExpr [$2] }
            | stup statement  expr etup                 { TupleExpr ((exprs $2) ++ [$3]) }
            | stup statements expr etup                 { TupleExpr ((exprs $2) ++ [$3]) }

fnCall      : fnDef block                               { FunDef (inpars $1) (fnid $1) (expars $1) $2 }
            | fnDef line                                { FunDef (inpars $1) (fnid $1) (expars $1) $2 }

fnDef       : tuple Id tuple DEF                        { FunDef $1 (AtmId $2) $3 Undefined }
            | tuple Id DEF                              { FunDef $1 (AtmId $2) Undefined Undefined }
            | Id tuple DEF                              { FunDef Undefined (AtmId $1) $2 Undefined }
            | Id DEF                                    { FunDef Undefined (AtmId $1) Undefined Undefined }

lambda      : ';' expr DEF                              { Lambda $2 Undefined }

block       : sdo expr edo                              { Statements [$2] }
            | sdo statement  edo                        { $2 }
            | sdo statements edo                        { $2 }
            | sdo statement  expr edo                   { Statements ((exprs $2) ++ [$3]) }
            | sdo statements expr edo                   { Statements ((exprs $2) ++ [$3]) }
            | sdo lines edo                             { $2 }
            | sdo line  edo                             { $2 }



-- Parser Primitives Below

literal     : TyId                      { DataType $ CoreType ModUnspecified $1}
            | '()'                      { DataType $ NilType ModUnspecified }
            | INT                       { Atoms (AtmInt $1) }
            | FLT                       { Atoms (AtmFlt $1) }
            | STR                       { Atoms (AtmStr $1) }

modTy       : '~'                       { Modifiers [Mutb] }
            | '@'                       { Modifiers [Refr] }
            | '[]'                      { Modifiers [Arry] }
            | '[' INT ']'               { Modifiers [ArSz $2]}
            | modTy modTy               { Modifiers ((mods $1) ++ (mods $2)) }

--Newlines need no functions. They exist to be tracked and then discarded.
nl          : '\n'                      { Undefined }
            | nl nl                     { Undefined }

stup        : '('                       { Undefined }
            | '(' nl                    { Undefined }

etup        : ')'                       { Undefined }
            | nl ')'                    { Undefined }

sdo         : '{'                       { Undefined }
            | '{' nl                    { Undefined }

edo         : '}'                       { Undefined }
            | nl '}'                    { Undefined }
            | '}' nl                    { Undefined }
            | nl '{' nl                 { Undefined }


{

parseError tokens = do
  let pos = if (length tokens >= 0)
              then (spos $ tokens !! 0)
              else (BzoPos 0 0 "Unknown File")
  error ("Parse Error while parsing " ++ (show $ fileName pos) ++ ", at " ++
    (show $ line pos) ++ ":" ++ (show $ column pos) ++ ". Tokens from rest of line:\n" ++ (show tokens))

}
