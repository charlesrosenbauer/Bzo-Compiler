
{
    module BzoParser where
    import BzoTypes
}










%name bzoParser
%tokentype { BzoToken }
%error { parseBzoError }










%token
    '('           { TkStartTup }
    ')'           { TkEndTup   }
    '['           { TkStartDat }
    ']'           { TkEndDat   }
    '{'           { TkStartDo  }
    '}'           { TkEndDo    }
    '.'           { TkSepExpr  }
    ','           { TkSepPoly  }
    









%%
Gr_Tuple : '(' Gr_Tuple 	{ TkTypeId }








