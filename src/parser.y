
{
    module BzoParser where
    import BzoTypes
}










%name bzoParser
%tokentype { BzoToken }










%token
    '('           { TkStartTup  }
    ')'           { TkEndTup    }
    '['           { TkStartDat  }
    ']'           { TkEndDat    }
    '{'           { TkStartDo   }
    '}'           { TkEndDo     }
    '.'           { TkSepExpr   }
    ','           { TkSepPoly   }
    ':'           { TkFilterSym }
    ';'           { TkLambdaSym }
    '~'           { TkMutable   }
    '@'           { TkReference }
    '_'           { TkWildcard  }
    '::'          { TkDefine    }
    ';;'          { TkFnSym     }
    '()'          { TkTupEmpt   }
    '[]'          { TkArrGnrl   }
    '{}'          { TkExpGnrl   }
    '..'          { TkArrMod    }
    '\n'          { TkNewline   }
    TyId          { TkTypeId    }
    INT           { TkInt       }
    FLT           { TkFlt       }
    STR           { TkStr       }









%%

--Type Parsing

TyDef    : TyId '::' Type           {  }

FnType   : Type ';;' Type           {  }

TypeTupl : '(' TyId ')'             {  }
         | '(' TyExpr ')'           {  }
         | '(' TypeTupl ')'         {  }

TyExpr   : TyId '.'                 {  }
         | TypeTupl '.'             {  }
         | TyExpr TyExpr            {  }
         | TyExpr TypeTupl          {  }

PolyTupl : '(' PlExpr ')'           {  }
         | '(' PlExpr TyId ')'      {  }
         | '(' PlExpr TypeTupl ')'  {  }

PlExpr   : TyId ','                 {  }
         | TypeTupl ','             {  }
         | PlExpr PlExpr            {  }

Type     : TypeTupl                 {  }
         | PolyTupl                 {  }
         | TyId                     {  }
         | '()'                     {  }

--Expression Parsing

Expr     : '(' AtomTrm  Atom ')'    {  }
         | '(' AtomTrms Atom ')'    {  }

AtomTrms : AtomTrm  AtomTrm         {  }
         | AtomTrms AtomTrm         {  }

AtomTrm  : Atom '.'                 {  }
         | Atom '\n'                {  }

--This should probably be the last one
Atom     : '(' Atom ')'             {  }
         | INT                      {  }
         | FLT                      {  }
         | STR                      {  }










