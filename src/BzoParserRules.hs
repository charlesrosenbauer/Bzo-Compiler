module BzoParserRules where
import BzoTypes
import Data.List as L
import Data.Text
import Debug.Trace










parserIter :: String -> [BzoSyntax] -> [BzoSyntax] -> Either [BzoErr] BzoSyntax

-- | Nothing to Parse?

parserIter fname [] [] = Left $ [ParseErr (BzoPos 1 1 fname) "Nothing to Parse?"]



-- | Simple reductions

parserIter fname tokens ((BzS_Token _ (TkNil)):stk)                     = parserIter fname tokens stk

parserIter fname tokens ((BzS_Token _ (TkNewline p1))
                        :(BzS_Token _ (TkNewline p0)):stk)              = parserIter fname tokens ((BzS_Token p0 (TkNewline p0):stk))

parserIter fname tokens ((BzS_Token _ (TkEndTup   p1))
                        :(BzS_Token _ (TkStartTup p0)):stk)             = parserIter fname tokens ((BzS_Expr p0 [BzS_Nil p0]):stk)

parserIter fname tokens ((BzS_Token _ (TkTupEmpt  p0)):stk)             = parserIter fname tokens ((BzS_Expr p0 [BzS_Nil p0]):stk)

parserIter fname tokens ((BzS_Token _ (TkEndDat   p1))
                        :(BzS_Token _ (TkStartDat p0)):stk)             = parserIter fname tokens ((BzS_ArrGenMod p0):stk)

parserIter fname tokens ((BzS_Token _ (TkNewline  p1))
                        :(BzS_Token _ (TkStartTup p0)):stk)             = parserIter fname tokens ((BzS_Token p0 (TkStartTup p0)):stk)

parserIter fname tokens ((BzS_Token _ (TkNewline  p1))
                        :(BzS_Token _ (TkStartDo  p0)):stk)             = parserIter fname tokens ((BzS_Token p0 (TkStartDo  p0)):stk)

parserIter fname tokens ((BzS_Token _ (TkEndTup   p1))
                        :(BzS_Token _ (TkNewline  p0)):stk)             = parserIter fname tokens ((BzS_Token p0 (TkEndTup   p0)):stk)

parserIter fname tokens ((BzS_Token _ (TkEndDo    p1))
                        :(BzS_Token _ (TkNewline  p0)):stk)             = parserIter fname tokens ((BzS_Token p0 (TkEndDo    p0)):stk)

parserIter fname tokens ((BzS_Token _ (TkNewline  p1))
                        :(BzS_Token _ (TkSepExpr  p0)):stk)             = parserIter fname tokens ((BzS_Token p0 (TkSepExpr  p0)):stk)

parserIter fname tokens ((BzS_Token _ (TkNewline  p1))
                        :(BzS_Token _ (TkSepPoly  p0)):stk)             = parserIter fname tokens ((BzS_Token p0 (TkSepPoly  p0)):stk)


-- | Core expression components

parserIter fname tokens ((BzS_Token _ (TkId       p0 fnid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_Id   p0 fnid]):stk)

parserIter fname tokens ((BzS_Token _ (TkBuiltin  p0 bfid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_BId  p0 bfid]):stk)

parserIter fname tokens ((BzS_Token _ (TkBIType   p0 btid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_BTId p0 btid]):stk)

parserIter fname tokens ((BzS_Token _ (TkMutId    p0 mtid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_MId  p0 mtid]):stk)

parserIter fname tokens ((BzS_Expr p1 [x])
                        :(BzS_Expr p0 xs)                   :stk)       = parserIter fname tokens ((BzS_Expr p0 (x:xs)):stk)

parserIter fname tokens ((BzS_Token _ (TkInt      p0  num)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_Int p0 num]):stk)

parserIter fname tokens ((BzS_Token _ (TkFlt      p0  num)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_Flt p0 num]):stk)

parserIter fname tokens ((BzS_Token _ (TkStr      p0  str)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_Str p0 str]):stk)

parserIter fname tokens ((BzS_Token _ (TkWildcard p0)) :stk)            = parserIter fname tokens ((BzS_Expr p0 [BzS_Wildcard p0]):stk)

parserIter fname tokens ((BzS_Token _ (TkEndTup p2))
                        :x@(BzS_Expr p1 _)
                        :(BzS_Token _ (TkStartTup p0)):stk)             = parserIter fname tokens ((BzS_Expr p0 [x]):stk)


-- | Statements and Blocks

parserIter fname tokens ((BzS_Token _ (TkNewline p1))
                        :x@(BzS_Expr p0 _)      :stk)                   = parserIter fname tokens ((BzS_Statement p0 x):stk)

parserIter fname tokens ((BzS_Token _ (TkNewline p1))
                        :(BzS_Statement p0 x)  :stk)                    = parserIter fname tokens ((BzS_Statement p0 x):stk)

parserIter fname tokens ((BzS_Statement p1 x)
                        :(BzS_Token _ (TkStartDo p0)):stk)              = parserIter fname tokens ((BzS_BlockHead p0 [x]):stk)

parserIter fname tokens ((BzS_Statement p1 x)
                        :(BzS_BlockHead p0 xs):stk)                     = parserIter fname tokens ((BzS_BlockHead p0 (x:xs)):stk)

parserIter fname tokens ((BzS_Token _ (TkEndDo p1))
                        :(BzS_BlockHead p0 xs):stk)                     = parserIter fname tokens ((BzS_Block p0 xs):stk)

parserIter fname tokens ((BzS_Token _ (TkEndDo p2))
                        :x@(BzS_Expr p1 _)
                        :(BzS_BlockHead p0 xs):stk)                     = parserIter fname tokens ((BzS_Block p0 (x:xs)):stk)

parserIter fname tokens ((BzS_Token _ (TkEndDo p2))
                        :x@(BzS_Expr p1 _)
                        :(BzS_Token _ (TkStartDo p0)):stk)              = parserIter fname tokens ((BzS_Block p0 [x]):stk)

-- | Expression Construction

parserIter fname tokens ((BzS_Token _ (TkSepExpr p2))
                        :x@(BzS_Expr  p1 _)
                        :(BzS_Token _ (TkStartTup p0)):stk)             = parserIter fname tokens ((BzS_CmpdHead p0 [x]):stk)

parserIter fname tokens ((BzS_Token _ (TkSepExpr p2))
                        :x@(BzS_Expr  p1 _)
                        :(BzS_CmpdHead p0 xs):stk)                      = parserIter fname tokens ((BzS_CmpdHead p0 (x:xs)):stk)

parserIter fname tokens ((BzS_Token _ (TkEndTup p2))
                        :x@(BzS_Expr  p1 _)
                        :(BzS_CmpdHead p0 xs):stk)                      = parserIter fname tokens ((BzS_Expr p0 [BzS_Cmpd p0 (x:xs)]):stk)

parserIter fname tokens ((BzS_Token _ (TkSepPoly p2))
                        :x@(BzS_Expr  p1 _)
                        :(BzS_Token _ (TkStartTup p0)):stk)             = parserIter fname tokens ((BzS_PolyHead p0 [x]):stk)

parserIter fname tokens ((BzS_Token _ (TkSepPoly p2))
                        :x@(BzS_Expr  p1 _)
                        :(BzS_PolyHead p0 xs):stk)                      = parserIter fname tokens ((BzS_PolyHead p0 (x:xs)):stk)

parserIter fname tokens ((BzS_Token _ (TkEndTup p2))
                        :x@(BzS_Expr  p1 _)
                        :(BzS_PolyHead p0 xs):stk)                      = parserIter fname tokens ((BzS_Expr p0 [BzS_Poly p0 (x:xs)]):stk)



-- | Lambda Expressions
parserIter fname tokens (xpr@(BzS_Block p2 _)
                        :(BzS_Expr _ [x@(BzS_Expr p1 _)])
                        :(BzS_Token _ (TkLambdaSym p0)):stk)            = parserIter fname tokens ((BzS_Lambda p0 x xpr):stk)

parserIter fname tokens (xpr@(BzS_Block p2 _)
                        :x@(BzS_Expr p1 [_])
                        :(BzS_Token _ (TkLambdaSym p0)):stk)            = parserIter fname tokens ((BzS_Lambda p0 x xpr):stk)

parserIter fname tokens (xpr@(BzS_Block  p2 _)
                        :x@(BzS_Cmpd p1 pars)
                        :(BzS_Token _ (TkLambdaSym p0)):stk)            = parserIter fname tokens ((BzS_Lambda p0 x xpr):stk)




-- | Control Logic

parserIter fname [] [item]        = Right item

parserIter fname [] (s:stack)     = Left [ParseErr (pos s) ("Parser could not consume entire file.\nStack:\n" ++ (L.concatMap (\s -> show s ++ "\n") (s:stack)))]

parserIter fname (t:tokens) stack = parserIter fname tokens (t:stack)
