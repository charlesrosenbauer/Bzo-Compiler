
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
    '::'          { TkDefine    _ }
    ';;'          { TkFnSym     _ }
    '()'          { TkTupEmpt   _ }
    '[]'          { TkArrGnrl   _ }
    '..'          { TkArrMod    _ }
    '\n'          { TkNewline   }
    TyId          { TkTypeId    _ $$ }
    Id            { TkId        _ $$ }
    INT           { TkInt       _ $$ }
    FLT           { TkFlt       _ $$ }
    STR           { TkStr       _ $$ }









%%

--call      : fn newlines               { $1 }
--          | expr newlines             { $1 }

--fn        : funDef stmnt              { FunDef (inpars $1) (fnid $1) (outpars $1) $2}
--          | funDef '{' stmnt '}'      { FunDef (inpars $1) (fnid $1) (outpars $1) $3 }
--          | funDef '{' lines '}'      { FunDef (inpars $1) (fnid $1) (outpars $1) $3 }
--          | funDef '{' stmnt expr '}' { FunDef (inpars $1) (fnid $1) (outpars $1) (Statements ((exprs $3) ++ [$4])) }
--          | funDef expr               { FunDef (inpars $1) (fnid $1) (outpars $1) (Statements [$2]) }
--          | funDef '{' expr '}'       { FunDef (inpars $1) (fnid $1) (outpars $1) (Statements [$3]) }
--          | funDef tyExpr             { Undefined }

--ty        : tyDef tyExpr              { Undefined }

--tyDef     : expr TyId '::'            { Undefined }
--          | TyId '::'                 { Undefined }

--funDef    : expr Id expr '::'         { FunDef $1 (AtmStr $2) $3 Undefined }
--          | Id expr '::'              { FunDef Undefined (AtmStr $1) $2 Undefined }
--          | expr Id '::'              { FunDef $1 (AtmStr $2) Undefined Undefined }
--          | Id '::'                   { FunDef Undefined (AtmStr $1) Undefined Undefined}

--lambda    : ';' expr '::'             { Lambda $2 Undefined }

--lines     : line line                 { Statements ((exprs $1) ++ (exprs $2)) }
--          | lines line                { Statements ((exprs $1) ++ (exprs $2)) }

--line      : stmnt newlines            { $1 }
--          | stmnt expr newlines       { Statements ((exprs $1) ++ [$2]) }

--stmnt     : expr '.'                  { Statements [$1] }
--          | stmnt stmnt               { Statements ((exprs $1) ++ (exprs $2)) }

--expr      : atom                      { Expr [$1] }
--          | expr atom                 { Expr ((exprs $1) ++ [$2]) }
--          | expr expr                 { Expr ((exprs $1) ++ (exprs $2)) }
--          | '(' expr ')'              { Expr [$2] }
--          | '(' stmnt ')'             { Expr [$2] }
--          | '(' stmnt expr ')'        { Expr ((exprs $2) ++ [$3]) }
--          | lambda '{' stmnt '}'      { Lambda (pars $1) $3 }
--          | lambda '{' lines '}'      { Lambda (pars $1) $3 }
--          | lambda '{' stmnt expr '}' { Lambda (pars $1) (Statements ((exprs $3) ++ [$4])) }
--          | lambda '{' expr '}'       { Lambda (pars $1) (Statements [$3]) }
--          | lambda expr               { Lambda (pars $1) (Statements [$2]) }
--          | lambda stmnt              { Lambda (pars $1) $2 }

--tyExpr    : tyAtom                    { Undefined }
--          | '(' tyStmnt ')'           { Undefined }
--          | '(' plStmnt ')'           { Undefined }
--          | '(' tyStmnt tyAtom ')'    { Undefined }
--          | '(' plStmnt tyAtom ')'    { Undefined }
--          | tyExpr ';;' tyExpr        { Undefined }
--          | modTy tyExpr              { Undefined }

--plStmnt   : tyAtom ','                { Undefined }
--          | tyAtom ',' newlines       { Undefined }
--          | plStmnt plStmnt           { Undefined }

--tyStmnt   : tyAtom '.'                { Undefined }
--          | tyAtom '.' newlines       { Undefined }
--          | tyStmnt tyStmnt           { Undefined }

--modTy     : '~'                       { Undefined }
--          | '@'                       { Undefined }
--          | '[]'                      { Undefined }
--          | '[' INT ']'               { Undefined }
--          | modTy modTy               { Undefined }

--Newlines need no functions. They exist to be tracked and then discarded.
--newlines  : '\n'                      { Undefined }
--          | newlines '\n'             { Undefined }
--          | newlines newlines         { Undefined }

--tyAtom    : TyId                      { Undefined }
--          | '()'                      { Undefined }

--atom      : INT                       { Atoms (AtmInt $1) }
--          | FLT                       { Atoms (AtmFlt $1) }
--          | STR                       { Atoms (AtmStr $1) }
--          | Id                        { Atoms (AtmId  $1) }




calls       : fn                        { Calls [$1] }
            | lines                     { Calls [$1] }
            | nl                        { Calls [] }
            | calls calls               { Calls ((calls $1) ++ (calls $2)) }

lines       : expr nl                   { Statements [$1] }
            | stmnt nl                  { $1 }
            | lines lines               { Statements ((exprs $1) ++ (exprs $2)) }

fn          : fnDef expr                { FunDef (inpars $1) (fnid $1) (expars $1) $2 }
            | fnDef stmnt               { FunDef (inpars $1) (fnid $1) (expars $1) $2 }
            | fnDef stexpr              { FunDef (inpars $1) (fnid $1) (expars $1) $2 }
            | fnDef expr nl             { FunDef (inpars $1) (fnid $1) (expars $1) $2 }
            | fnDef stmnt nl            { FunDef (inpars $1) (fnid $1) (expars $1) $2 }
            | fnDef stexpr nl           { FunDef (inpars $1) (fnid $1) (expars $1) $2 }
            | fnDef sdo expr edo        { FunDef (inpars $1) (fnid $1) (expars $1) $3 }
            | fnDef sdo stmnt edo       { FunDef (inpars $1) (fnid $1) (expars $1) $3 }
            | fnDef sdo stexpr edo      { FunDef (inpars $1) (fnid $1) (expars $1) $3 }
            | fnDef sdo lines edo       { FunDef (inpars $1) (fnid $1) (expars $1) $3 }

fnDef       : expr Id expr '::'         { FunDef $1 (AtmId $2) $3 Undefined }
            | expr Id '::'              { FunDef $1 (AtmId $2) Undefined Undefined }
            | Id expr '::'              { FunDef Undefined (AtmId $1) $2 Undefined }
            | Id '::'                   { FunDef Undefined (AtmId $1) Undefined Undefined }

lambda      : ';' expr '::'             { Lambda $2 Undefined }

expr        : atom                      { Expr [$1] }
            | expr expr                 { Expr ((exprs $1) ++ (exprs $2)) }
            | stup expr etup            { Expr [$2] }
            | stup stmnt etup           { Expr (exprs $2) }
            | stup stexpr etup          { Expr [$2] }
            | lambda expr               { Lambda (pars $1) $2 }
            | lambda stmnt              { Lambda (pars $1) $2 }
            | lambda stexpr             { Lambda (pars $1) $2 }
            | lambda sdo expr edo       { Lambda (pars $1) $3 }
            | lambda sdo stmnt edo      { Lambda (pars $1) $3 }
            | lambda sdo stexpr edo     { Lambda (pars $1) $3 }
            | lambda sdo lines edo      { Lambda (pars $1) $3 }

stmnt       : expr '.'                  { Statements [$1] }
            | stmnt stmnt               { Statements ((exprs $1) ++ (exprs $2)) }

stexpr      : stmnt expr                { Statements ((exprs $1) ++ [$2]) }

modTy       : '~'                       { Modifiers [Mutb] }
            | '@'                       { Modifiers [Refr] }
            | '[]'                      { Modifiers [Arry] }
            | '[' INT ']'               { Modifiers [ArSz $2]}
            | modTy modTy               { Modifiers ((mods $1) ++ (mods $2)) }

--Newlines need no functions. They exist to be tracked and then discarded.
nl          : '\n'                      { Undefined }
            | nl nl                     { Undefined }

tyAtom      : TyId                      { Undefined }
            | '()'                      { Undefined }

atom        : INT                       { Atoms (AtmInt $1) }
            | FLT                       { Atoms (AtmFlt $1) }
            | STR                       { Atoms (AtmStr $1) }
            | Id                        { Atoms (AtmId  $1) }
            | Id '..'                   { ArrAtoms (AtmId $1) }
            | '_'                       { Wildcard }

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

parseError tokens = error ("\n\n\n\n" ++ (show tokens))

}
