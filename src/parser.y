
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
    TyId          { TkTypeId    }









%%
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



