
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
    FDEF          { TkFnSym     _ }
    '()'          { TkTupEmpt   _ }
    '[]'          { TkArrGnrl   _ }
    '..'          { TkArrMod    _ }
    '\n'          { TkNewline   _ }
    TyId          { TkTypeId    _ $$ }
    Id            { TkId        _ $$ }
    INT           { TkInt       _ $$ }
    FLT           { TkFlt       _ $$ }
    STR           { TkStr       _ $$ }
    BI            { TkBuiltin   _ $$ }
    BIT           { TkBIType    _ $$ }









%%

calls       : fnCall                                    { Calls [$1] }
            | typeDef                                   { Calls [$1] }
            | fnTypeDef                                 { Calls [$1] }
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
            | TyId                                      { Expr [Constructor $1 ModUnspecified] }
            | modTy TyId                                { Expr [Constructor $2 $1] }
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
            | expr ':' type                             { Expr  }   -- Need to add some stuff for this
            | BIT                                       { Expr  }   --
            | BI                                        { Expr  }   --

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

typeDef     : TyId DEF type                             { TypDef Undefined $1 $3 }
            | expr TyId DEF type                        { TypDef $1 $2 $4 }

typ         : TyId                                      { DataType $ DtCoreType $1 }
            | stup typ     etup                         { DataType $ DtTuple (typ $2) }
            | stup cptypes etup                         { $2 }
            | stup pmtypes etup                         { $2 }
            | modTy typ                                 { DataType $ DtModded $1 (typ $2) }
            | fnType                                    { $1 }
            | BIT                                       { DataType $ DtBIType $1 }
            | '()'                                      { DataType $ DtNilType }

record      : sdo memberDef  edo                        { $2 }
            | sdo memberDefs edo                        { $2 }

cptypes     : typ sepc                                  { DataType $ DtTuple $ typ $1 }
            | cptypes sepc typ                          { DataType $ DtTuple ((typs $ typ $1) ++ (typs $ typ $2)) }

pmtypes     : typ sepp                                  { DataType $ DtPolymorph $ typ $1 }
            | pmtypes sepp typ                          { DataType $ DtPolymorph ((typs $ typ $1) ++ (typs $ typ $2)) }

memberDef   : Id DEF typ                                { DataType $ DtRecord [RecUnit $1 $3] }

memberDefs  : memberDef  sepc memberDef                 { DataType $ DtRecord ((recs $ typ $1) ++ (recs $ typ $3)) }
            | memberDefs sepc memberDef                 { DataType $ DtRecord ((recs $ typ $1) ++ (recs $ typ $3)) }

fnType      : typ FDEF typ                              { DataType $ DtFunc (typ $1) (typ $3) }

fnTypeDef   : Id FDEF fnType                            { FnTypeDef $1 (tyIn $3) (tyEx $3) }

-- Parser Primitives Below

literal     : TyId                      { DataType $ DtCoreType $1}
            | '()'                      { DataType $ DtNilType }
            | INT                       { Atoms (AtmInt $1) }
            | FLT                       { Atoms (AtmFlt $1) }
            | STR                       { Atoms (AtmStr $1) }

modTy       : '~'                       { Modifiers [Mutb] }
            | '@'                       { Modifiers [Refr] }
            | '[]'                      { Modifiers [Arry] }
            | '[' INT ']'               { Modifiers [ArSz $2]}
            | '[' Id  ']'               { Modifiers [ArVr $2]}
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

sepc        : '.'                       { Undefined }
            | '.' nl                    { Undefined }
            | nl '.'                    { Undefined }
            | nl '.' nl                 { Undefined }

sepp        : ','                       { Undefined }
            | ',' nl                    { Undefined }
            | nl ','                    { Undefined }
            | nl ',' nl                 { Undefined }

{

parseError tokens = do
  let pos = if (length tokens > 0)
              then (spos $ tokens !! 0)
              else (BzoPos 0 0 "Unknown File")
  error ("Parse Error while parsing " ++ (show $ fileName pos) ++ ", at " ++
    (show $ line pos) ++ ":" ++ (show $ column pos) ++ ". Tokens from rest of line:\n" ++ (show tokens))

}
