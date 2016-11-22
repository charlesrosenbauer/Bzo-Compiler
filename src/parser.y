
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

fn        : funDef stmnt              {  }
          | funDef '{' stmnt '}'      {  }
          | funDef '{' lines '}'      {  }
          | funDef '{' stmnt expr '}' {  }

funDef    : expr Id expr '::'         {  }
          | Id expr '::'              {  }
          | expr Id '::'              {  }
          | Id '::'                   {  }

lines     : stmnt newlines            {  }
          | stmnt expr newlines       {  }

do        : stmnt stmnt               {  }
          | do stmnt                  {  }

stmnt     : expr '.'                  {  }
          | stmnt stmnt               {  }

expr      : atom                      {  }
          | expr atom                 {  }
          | expr expr                 {  }
          | '(' expr ')'              {  }
          | '(' do ')'                {  }
          | '(' do expr ')'           {  }

newlines  : '\n'                      {  }
          | newlines '\n'             {  }
          | newlines newlines         {  }

atom      : INT                       { Atoms [(AtmInt $1)] }
          | FLT                       { Atoms [(AtmFlt $1)] }
          | STR                       { Atoms [(AtmStr $1)] }
          | Id                        { Atoms [(AtmId  $1)] }









{

parseError tokens = error $ show tokens

}
