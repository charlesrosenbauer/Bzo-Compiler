
{
    module BzoParser where
    import BzoTypes
    
    data Gr_Tuple
        = ..
        | ..
        | ..
}










%name bzoParser
%tokentype { BzoToken }
%monad { BzoToken } { (>>=) } { return }
%error { parseBzoError }










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









%%
Gr_Tuple : '(' Gr_Tuple ')'	{ TkTypeId }








