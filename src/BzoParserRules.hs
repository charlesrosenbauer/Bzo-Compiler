module BzoParserRules where
import BzoTypes
import Data.List as L
import Data.Text
import Debug.Trace










parserIter :: String -> [BzoSyntax] -> [BzoSyntax] -> Either [BzoErr] BzoSyntax

parserIter fname [] [] = Left $ [ParseErr (BzoPos 1 1 fname) "Nothing to Parse?"]

parserIter fname tokens ((BzS_Token _ (TkNil)):stk)               = parserIter fname tokens stk

parserIter fname tokens ((BzS_Token _ (TkNewline p1))
                        :(BzS_Token _ (TkNewline p0)):stk)        = parserIter fname tokens ((BzS_Token p0 (TkNewline p0):stk))

parserIter fname tokens ((BzS_Token _ (TkEndTup   p1))
                        :(BzS_Token _ (TkStartTup p0)):stk)       = parserIter fname tokens ((BzS_Nil p0):stk)

parserIter fname tokens ((BzS_Token _ (TkEndDat   p1))
                        :(BzS_Token _ (TkStartDat p0)):stk)       = parserIter fname tokens ((BzS_ArrGenMod p0):stk)

parserIter fname tokens ((BzS_Token _ (TkId       p0 fnid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [(BzS_Id   p0 fnid)]):stk)

parserIter fname tokens ((BzS_Token _ (TkBuiltin  p0 bfid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [(BzS_BId  p0 bfid)]):stk)

parserIter fname tokens ((BzS_Token _ (TkBIType   p0 btid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [(BzS_BTId p0 btid)]):stk)

parserIter fname tokens ((BzS_Token _ (TkMutId    p0 mtid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [(BzS_MId  p0 mtid)]):stk)

parserIter fname tokens ((BzS_Expr p1 [x])
                        :(BzS_Expr p0 xs)                   :stk)       = parserIter fname tokens ((BzS_Expr p0 (x:xs)):stk)




parserIter fname [] [item]        = Right item

parserIter fname [] (s:stack)     = Left [ParseErr (pos s) ("Parser could not consume entire file.\nStack:\n" ++ (L.concatMap (\s -> show s ++ "\n") (s:stack)))]

parserIter fname (t:tokens) stack = parserIter fname tokens (t:stack)
