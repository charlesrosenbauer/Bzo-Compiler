
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

--doBlk     : '{' stmnts '}'           {  }
--          | '{' stmnt  '}'           {  }

--func      : fndef stmnt             {  }
--          | fndef doBlk             {  }

--fndef     : expr atom expr '::'      {  }
--          | expr atom '::'           {  }
--          | atom expr '::'           {  }
--          | atom '::'                {  }

stmnts    : stmnt  stmnt             {  }
          | stmnts stmnt             {  }

stmnt     : expr '.'                 {  }
          | expr newlines            {  }

expr      : expr expr                {  }
          | atom                     {  }

newlines  : '\n'                     {  }
          | newlines '\n'            {  }
          | newlines newlines        {  }

atoms     : atoms  atoms             { Atoms ((vals $1) ++ (vals $2)) }
          | atom atom                { Atoms ([$1] ++ [$2]) }

atom      : INT                      { Atoms [(AtmInt $1)] }
          | FLT                      { Atoms [(AtmFlt $1)] }
          | STR                      { Atoms [(AtmStr $1)] }
          | Id                       { Atoms [(AtmId  $1)] }









{

parseError tokens = error $ show tokens

}
