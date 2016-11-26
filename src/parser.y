
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
    '{}'          { TkExpGnrl   _ }
    '..'          { TkArrMod    _ }
    '\n'          { TkNewline     }
    TyId          { TkTypeId    _ $$ }
    Id            { TkId        _ $$ }
    INT           { TkInt       _ $$ }
    FLT           { TkFlt       _ $$ }
    STR           { TkStr       _ $$ }









%%

call      : fn newlines               { $1 }

fn        : funDef stmnt              { FunDef (inpars $1) (fnid $1) (outpars $1) $2}
          | funDef '{' stmnt '}'      { FunDef (inpars $1) (fnid $1) (outpars $1) $3 }
          | funDef '{' lines '}'      { FunDef (inpars $1) (fnid $1) (outpars $1) $3 }
          | funDef '{' stmnt expr '}' { FunDef (inpars $1) (fnid $1) (outpars $1) (Statements ((exprs $3) ++ [$4])) }
          | funDef expr               { FunDef (inpars $1) (fnid $1) (outpars $1) (Statements [$2]) }
          | funDef '{' expr '}'       { FunDef (inpars $1) (fnid $1) (outpars $1) (Statements [$3]) }
          | funDef tyExpr             {}

ty        : tyDef tyExpr              {}

tyDef     : expr TyId '::'            {}
          | TyId '::'                 {}

funDef    : expr Id expr '::'         { FunDef $1 (AtmStr $2) $3 Undefined }
          | Id expr '::'              { FunDef Undefined (AtmStr $1) $2 Undefined }
          | expr Id '::'              { FunDef $1 (AtmStr $2) Undefined Undefined }
          | Id '::'                   { FunDef Undefined (AtmStr $1) Undefined Undefined}

lambda    : ';' expr '::'             { Lambda $2 Undefined }

lines     : line line                 { Statements ((exprs $1) ++ (exprs $2)) }
          | lines line                { Statements ((exprs $1) ++ (exprs $2)) }

line      : stmnt newlines            { $1 }
          | stmnt expr newlines       { Statements ((exprs $1) ++ [$2]) }

stmnt     : expr '.'                  { Statements [$1] }
          | stmnt stmnt               { Statements ((exprs $1) ++ (exprs $2)) }

expr      : atom                      { Expr [$1] }
          | expr atom                 { Expr ((exprs $1) ++ [$2]) }
          | expr expr                 { Expr ((exprs $1) ++ (exprs $2)) }
          | '(' expr ')'              { Expr [$2] }
          | '(' stmnt ')'             { Expr [$2] }
          | '(' stmnt expr ')'        { Expr ((exprs $2) ++ [$3]) }
          | lambda '{' stmnt '}'      { Lambda (pars $1) $3 }
          | lambda '{' lines '}'      { Lambda (pars $1) $3 }
          | lambda '{' stmnt expr '}' { Lambda (pars $1) (Statements ((exprs $3) ++ [$4])) }
          | lambda '{' expr '}'       { Lambda (pars $1) (Statements [$3]) }
          | lambda expr               { Lambda (pars $1) (Statements [$2]) }
          | lambda stmnt              { Lambda (pars $1) $2 }

tyExpr    : tyAtom                    {  }
          | '(' tyExpr ')'            {  }
          | '(' plExpr ')'            {  }
          | '(' tyExpr tyAtom ')'     {  }
          | '(' plExpr tyAtom ')'     {  }
          | tyExpr ';;' tyExpr        {  }
          | modTy tyExpr              {  }

plStmnt   : tyId ','                  {  }
          | tyId ',' newlines         {  }
          | plStmnt plStmnt           {  }

tyStmnt   : tyId '.'                  {  }
          | tyId '.' newlines         {  }
          | tyStmnt tyStmnt           {  }

modTy     : '~'                       {  }
          | '@'                       {  }
          | '[]'                      {  }
          | '[' INT ']'               {  }
          | modTy modTy               {  }

--Newlines need no functions. They exist to be tracked and then discarded.
newlines  : '\n'                      {  }
          | newlines '\n'             {  }
          | newlines newlines         {  }

tyAtom    : TyId                      {  }
          | '()'                      {  }
          | '{}'                      {  }

atom      : INT                       { Atoms [(AtmInt $1)] }
          | FLT                       { Atoms [(AtmFlt $1)] }
          | STR                       { Atoms [(AtmStr $1)] }
          | Id                        { Atoms [(AtmId  $1)] }









{

parseError tokens = error $ show tokens

}
