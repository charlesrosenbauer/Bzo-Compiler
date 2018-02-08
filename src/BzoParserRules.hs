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

parserIter fname tokens ((BzS_Token _ (TkEndDat   p2))
                        :(BzS_Expr  _ [BzS_Int    p1 sz])
                        :(BzS_Token _ (TkStartDat p0)):stk)             = parserIter fname tokens ((BzS_ArrSzObj p0 sz):stk)

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
parserIter fname tokens ((BzS_Token _ (TkId       p0 fnid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_Id    p0 fnid]):stk)

parserIter fname tokens ((BzS_Token _ (TkBuiltin  p0 bfid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_BId   p0 bfid]):stk)

parserIter fname tokens ((BzS_Token _ (TkTypeId   p0 tyid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_TyId  p0 tyid]):stk)

parserIter fname tokens ((BzS_Token _ (TkBIType   p0 btid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_BTId  p0 btid]):stk)

parserIter fname tokens ((BzS_Token _ (TkMutId    p0 mtid)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_MId   p0 mtid]):stk)

parserIter fname tokens ((BzS_Token _ (TkTyVar    p0 tyvr)) :stk)       = parserIter fname tokens ((BzS_Expr p0 [BzS_TyVar p0 tyvr]):stk)

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

parserIter fname tokens (ty1@(BzS_Expr  p2 xs1)
                        :(BzS_Token p1 (TkFnSym _))
                        :ty0@(BzS_Expr  p0 xs0)  :stk)                  = parserIter fname tokens ((BzS_Expr p0 [BzS_FnTy p0 ty0 ty1]):stk)

parserIter fname tokens ((BzS_Expr p1 xs)
                        :(BzS_Expr p0 [BzS_FnTy _ t0 (BzS_Expr p t1)])
                        :stk)                                           = parserIter fname tokens ((BzS_Expr p0 [BzS_FnTy p0 t0 (BzS_Expr p $ t1++xs)]):stk)

parserIter fname tokens ((BzS_Expr p1 [BzS_FnTy _ (BzS_Expr p t0) t1])
                        :(BzS_Expr p0 xs)
                        :stk)                                           = parserIter fname tokens ((BzS_Expr p0 [BzS_FnTy p0 (BzS_Expr p $ xs++t0) t1]):stk)

parserIter fname tokens ((BzS_Expr p1 [x])
                        :(BzS_Expr p0 xs)                   :stk)       = parserIter fname tokens ((BzS_Expr p0 (x:xs)):stk)



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



-- | Function Types
parserIter fname tokens ((BzS_Statement p1 (BzS_Expr _ [def@(BzS_FnTy _ inty exty)]))
                        :(BzS_FnHead p0 BzS_Undefined fn BzS_Undefined):stk)    = parserIter fname tokens ((BzS_Calls p0 [BzS_FnTypeDef p0 BzS_Undefined fn def]):stk)

parserIter fname tokens ((BzS_Statement p1 (BzS_Expr _ [def@(BzS_FnTy _ inty exty)]))
                        :(BzS_FnHead p0 inpars        fn BzS_Undefined):stk)    = parserIter fname tokens ((BzS_Calls p0 [BzS_FnTypeDef p0 inpars fn def]):stk)



-- | Functions
parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [expar@(BzS_Expr _ _),
                                             (BzS_Id _ fn ),
                                       inpar@(BzS_Expr _ _)])  :stk)    = parserIter fname tokens ((BzS_FnHead p0 inpar fn expar):stk)

parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [expar@(BzS_Expr _ _),
                                             (BzS_Id _ fn ),
                                       inpar@(BzS_Cmpd _ _)])  :stk)    = parserIter fname tokens ((BzS_FnHead p0 inpar fn expar):stk)

parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [expar@(BzS_Cmpd _ _),
                                             (BzS_Id _ fn ),
                                       inpar@(BzS_Expr _ _)])  :stk)    = parserIter fname tokens ((BzS_FnHead p0 inpar fn expar):stk)

parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [expar@(BzS_Cmpd _ _),
                                             (BzS_Id _ fn ),
                                       inpar@(BzS_Cmpd _ _)])  :stk)    = parserIter fname tokens ((BzS_FnHead p0 inpar fn expar):stk)

parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [      (BzS_Id _ fn ),
                                       inpar@(BzS_Expr _ _)])  :stk)    = parserIter fname tokens ((BzS_FnHead p0 inpar fn BzS_Undefined):stk)

parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [      (BzS_Id _ fn ),
                                       inpar@(BzS_Cmpd _ _)])  :stk)    = parserIter fname tokens ((BzS_FnHead p0 inpar fn BzS_Undefined):stk)

parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [expar@(BzS_Cmpd _ _),
                                             (BzS_Id _ fn )])  :stk)    = parserIter fname tokens ((BzS_FnHead p0 BzS_Undefined fn expar):stk)

parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [expar@(BzS_Expr _ _),
                                             (BzS_Id _ fn )])  :stk)    = parserIter fname tokens ((BzS_FnHead p0 BzS_Undefined fn expar):stk)

parserIter fname tokens ((BzS_Token p0 (TkDefine _))
                        :(BzS_Expr p1 [(BzS_Id _ fn )])        :stk)    = parserIter fname tokens ((BzS_FnHead p0 BzS_Undefined fn BzS_Undefined):stk)

parserIter fname tokens (def@(BzS_Block p1 xs)
                        :(BzS_FnHead p0 ins fn exs)            :stk)    = parserIter fname tokens ((BzS_Calls p0 [BzS_FunDef p0 ins fn exs def]):stk)

parserIter fname tokens (def@(BzS_Statement p1 xs)
                        :(BzS_FnHead p0 ins fn exs)            :stk)    = parserIter fname tokens ((BzS_Calls p0 [BzS_FunDef p0 ins fn exs def]):stk)



-- | Type Class Definitions
parserIter fname tokens ((BzS_Token  p1 (TkStartDo _))
                        :(BzS_TyHead p0 ins ty)                 :stk)   = parserIter fname tokens ((BzS_TyClassHead p0 ins ty []):stk)

parserIter fname tokens ((BzS_Token p1 (TkNewline _))
                        :tc@(BzS_TyClassHead p0 inpar ty fs)     :stk)  = parserIter fname tokens (tc:stk)

parserIter fname tokens ((BzS_Calls p1 [ft@(BzS_FnTypeDef _ _ _ _)])
                        :(BzS_TyClassHead p0 inpar ty fs)     :stk)     = parserIter fname tokens ((BzS_TyClassHead p0 inpar ty (ft:fs)):stk)

parserIter fname tokens ((BzS_Token p1 (TkEndDo _))
                        :(BzS_TyClassHead p0 inpar ty fs)     :stk)     = parserIter fname tokens ((BzS_Calls p0 [BzS_TyClassDef p0 inpar ty fs]):stk)



-- | Type Definitions
parserIter fname tokens ((BzS_Token p1 (TkDefine _))
                        :(BzS_Expr p0 [(BzS_TyId _ ty ),
                                        inpar@(BzS_Cmpd _ _)]):stk)     = parserIter fname tokens ((BzS_TyHead p0 inpar ty):stk)

parserIter fname tokens ((BzS_Token p1 (TkDefine _))
                        :(BzS_Expr p0 [(BzS_TyId _ ty ),
                                       inpar@(BzS_Expr _ _)])  :stk)    = parserIter fname tokens ((BzS_TyHead p0 inpar ty):stk)

parserIter fname tokens ((BzS_Token p1 (TkDefine _))
                        :(BzS_Expr p0 [(BzS_TyId _ ty )])        :stk)  = parserIter fname tokens ((BzS_TyHead p0 BzS_Undefined ty):stk)

parserIter fname tokens (def@(BzS_Statement p1 xs)
                        :(BzS_TyHead p0 ins ty)                 :stk)   = parserIter fname tokens ((BzS_Calls p0 [BzS_TypDef p0 ins ty def]):stk)




-- | Miscellaneous Parsing Rules - Curry, Map, Filter, Namespaces
parserIter fname tokens ((BzS_Token p1 (TkArrMod _))
                        :(BzS_Expr  p0 (x:xs))                  :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_MapObj (pos x) x):xs)):stk)

parserIter fname tokens ((BzS_Expr  p2 [y])
                        :(BzS_Token p1 (TkCurrySym _))
                        :(BzS_Expr  p0 ((BzS_CurryObj p x ys):xs))
                        :stk)                                           = parserIter fname tokens ((BzS_Expr p0 ((BzS_CurryObj p x (y:ys)):xs)):stk)

parserIter fname tokens ((BzS_Expr  p2 [y])
                        :(BzS_Token p1 (TkCurrySym _))
                        :(BzS_Expr  p0 (x:xs))                  :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_CurryObj (pos x) x [y]):xs)):stk)

parserIter fname tokens ((BzS_Expr  p2 [BzS_TyId _ ns])
                        :(BzS_Token p1 (TkReference _))
                        :(BzS_Expr  p0 ((BzS_Id p nm):xs))      :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_ExFunObj p nm ns):xs)):stk)

parserIter fname tokens ((BzS_Expr  p2 [BzS_TyId _ ns])
                        :(BzS_Token p1 (TkReference _))
                        :(BzS_Expr  p0 ((BzS_TyId p nm):xs))    :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_ExTypObj p nm ns):xs)):stk)

parserIter fname tokens ((BzS_Expr  p2 [BzS_TyId _ ns])
                        :(BzS_Token p1 (TkReference _))
                        :(BzS_Expr  p0 ((BzS_FilterObj p x ((BzS_TyId p' nm):ys)):xs))
                        :stk)                                           = parserIter fname tokens ((BzS_Expr p0 ((BzS_FilterObj p x ((BzS_ExTypObj p' nm ns):ys)):xs)):stk)

parserIter fname tokens ((BzS_Expr  p1 ((BzS_ArrayObj _ x ys):xs))
                        :(BzS_Token p0 (TkArrGnrl _))           :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_ArrayObj p0 x (0:ys)):xs)):stk)

parserIter fname tokens ((BzS_Expr  p1 ((BzS_ArrayObj _ x ys):xs))
                        :(BzS_ArrSzObj p0 sz)                   :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_ArrayObj p0 x (sz:ys)):xs)):stk)

parserIter fname tokens ((BzS_Expr  p1 (x:xs))
                        :(BzS_Token p0 (TkArrGnrl _))           :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_ArrayObj p0 x [0] ):xs)):stk)

parserIter fname tokens ((BzS_Expr  p1 (x:xs))
                        :(BzS_ArrSzObj p0 sz)                   :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_ArrayObj p0 x [sz]):xs)):stk)

parserIter fname tokens ((BzS_Expr  p2 [y])
                        :(BzS_Token p1 (TkFilterSym _))
                        :(BzS_Expr  p0 ((BzS_FilterObj p x tys):xs))
                        :stk)                                           = parserIter fname tokens ((BzS_Expr p0 ((BzS_FilterObj p x (y:tys)):xs)):stk)

parserIter fname tokens ((BzS_Expr  p2 [y])
                        :(BzS_Token p1 (TkFilterSym _))
                        :(BzS_Expr  p0 (x:xs))                  :stk)   = parserIter fname tokens ((BzS_Expr p0 ((BzS_FilterObj p0 x [y]):xs)):stk)



-- | Hints and Headers
parserIter fname tokens ((BzS_Token  p1 (TkNewline _))
                        :f@(BzS_File p0 mname _     incs imps df):stk)  = parserIter fname tokens (f:stk)

parserIter fname tokens (f@(BzS_File p1 mname _     incs imps df)
                        :(BzS_Token  p0 (TkNewline _))           :stk)  = parserIter fname tokens (f:stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [
                                            (BzS_TyId _ impas),
                                            (BzS_BTId _ "$ImportAs"),
                                            (BzS_TyId _ imp)])) :stk)   = parserIter fname tokens ((BzS_Import p0 imp impas):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [
                                            (BzS_BTId _ "$ImportAs"),
                                            (BzS_TyId _ imp)])) :stk)   = Left [ParseErr p0 "Attempting to import module with unspecified namespace."]

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [_,
                                            (BzS_BTId _ "$ImportAs"),
                                            (BzS_TyId _ imp)])) :stk)   = Left [ParseErr p0 "Attempting to import module with invalid namespace."]

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [
                                            (BzS_BTId _ "$Import"),
                                            (BzS_TyId _ imp)])) :stk)   = parserIter fname tokens ((BzS_Import p0 imp imp):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [_,
                                            (BzS_BTId _ "$Import"),
                                            (BzS_TyId _ imp)])) :stk)   = Left [ParseErr p0 "Invalid import call. Did you mean to use $ImportAs?"]

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [
                                            (BzS_TyId _ impas),
                                            (BzS_BTId _ "$IncludeAs"),
                                            (BzS_TyId _ imp)])) :stk)   = parserIter fname tokens ((BzS_Include p0 imp impas):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [
                                            (BzS_BTId _ "$Include"),
                                            (BzS_TyId _ imp)])) :stk)   = parserIter fname tokens ((BzS_Include p0 imp imp):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [
                                            (BzS_BTId _ "$IncludeAs"),
                                            (BzS_TyId _ imp)])) :stk)   = Left [ParseErr p0 "Attempting to include module with unspecified namespace."]

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [_,
                                            (BzS_BTId _ "$IncludeAs"),
                                            (BzS_TyId _ imp)])) :stk)   = Left [ParseErr p0 "Attempting to include module with invalid namespace."]

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [_,
                                            (BzS_BTId _ "$Include"),
                                            (BzS_TyId _ imp)])) :stk)   = Left [ParseErr p0 "Invalide include call. Did you mean to use $IncludeAs?"]

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr  _ [
                                            (BzS_BTId _ "$Module"),
                                            (BzS_TyId _ mname)])):stk)  = parserIter fname tokens ((BzS_File p0 mname fname [] [] []):stk)

parserIter fname tokens (i@(BzS_Include p1 imp impas)
                        :(BzS_File p0 mname _       incs imps []):stk)  = parserIter fname tokens ((BzS_File p0 mname fname (i:incs) imps []):stk)

parserIter fname tokens (i@(BzS_Include p1 imp impas)
                        :(BzS_File p0 mname _       incs imps df):stk)  = Left [ParseErr p1 "Include call found outside of file header."]

parserIter fname tokens (i@(BzS_Import p1 imp impas)
                        :(BzS_File p0 mname _       incs imps []):stk)  = parserIter fname tokens ((BzS_File p0 mname fname incs (i:imps) []):stk)

parserIter fname tokens (i@(BzS_Import p1 imp impas)
                        :(BzS_File p0 mname _       incs imps []):stk)  = Left [ParseErr p1 "Import call found outside of file header."]

parserIter fname tokens ((BzS_Calls  p1 calls)
                        :(BzS_File p0 mname _       incs imps df):stk)  = parserIter fname tokens ((BzS_File p0 mname fname incs imps (calls++df)):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr _ [
                                              (BzS_BTId _ h)]))  :stk)  = parserIter fname tokens ((BzS_TyHint p0 BzS_Undefined h BzS_Undefined):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr _ [
                                              xprex,
                                              (BzS_BTId _ h)]))  :stk)  = parserIter fname tokens ((BzS_TyHint p0 BzS_Undefined h xprex):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr _ [
                                              (BzS_BTId _ h),
                                              xprin         ]))  :stk)  = parserIter fname tokens ((BzS_TyHint p0 xprin h BzS_Undefined):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr _ [
                                              xprex,
                                              (BzS_BTId _ h),
                                              xprin         ]))  :stk)  = parserIter fname tokens ((BzS_TyHint p0 xprin h xprex):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr _ [
                                              (BzS_BId  _ h)]))  :stk)  = parserIter fname tokens ((BzS_FnHint p0 BzS_Undefined h BzS_Undefined):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr _ [
                                              xprex,
                                              (BzS_BId  _ h)]))  :stk)  = parserIter fname tokens ((BzS_FnHint p0 BzS_Undefined h xprex):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr _ [
                                              (BzS_BId  _ h),
                                              xprin         ]))  :stk)  = parserIter fname tokens ((BzS_FnHint p0 xprin h BzS_Undefined):stk)

parserIter fname tokens ((BzS_Statement p0 (BzS_Expr _ [
                                              xprex,
                                              (BzS_BId  _ h),
                                              xprin         ]))  :stk)  = parserIter fname tokens ((BzS_FnHint p0 xprin h xprex):stk)




-- | Generic Call Rules
parserIter fname tokens ((BzS_Token  p1 (TkNewline _))
                        :(BzS_Calls  p0 calls)                 :stk)    = parserIter fname tokens ((BzS_Calls p0 calls):stk)

parserIter fname tokens ((BzS_Calls  p1 calls1)
                        :(BzS_Calls  p0 calls0)                :stk)    = parserIter fname tokens ((BzS_Calls p0 (calls1++calls0)):stk)



-- | Control Logic

parserIter fname [] [item]        = Right item

parserIter fname [] (s:stack)     = Left (ParseErr (pos s) ("Parser could not consume entire file.\n"):(parserErr fname (L.reverse (s:stack)) []))

parserIter fname (t:tokens) stack = parserIter fname tokens (t:stack)










parserErr :: String -> [BzoSyntax] -> [BzoSyntax] -> [BzoErr]


-- | Nothing to Parse?

parserErr fname [] [] = [ParseErr (BzoPos 1 1 fname) "Nothing to Parse?"]

-- | Errors

parserErr fname (n:nxt) stk@((BzS_Token _ (TkSepPoly p2))
                             :x@(BzS_Expr  p1 _)
                             :(BzS_CmpdHead p0 xs):_)                = (ParseErr p0 "Unexpected comma (,) in compound tuple."    ):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token _ (TkSepExpr p2))
                             :x@(BzS_Expr  p1 _)
                             :(BzS_PolyHead p0 xs):_)                = (ParseErr p0 "Unexpected period (.) in polymorphic tuple."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Calls p1 calls1)
                             :xpr:_)                                 = (ParseErr (pos xpr) "Unexpected random expression among calls."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token _ (TkEndDat   p2))
                             :(BzS_Expr  _ [BzS_Flt    p1 num])
                             :(BzS_Token _ (TkStartDat p0)):_)       = (ParseErr p1 "Floats cannot be used to denote the size of an array."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token _ (TkEndDat   p2))
                             :(BzS_Expr  _ [BzS_Str    p1 str])
                             :(BzS_Token _ (TkStartDat p0)):_)       = (ParseErr p1 "Strings cannot be used to denote the size of an array."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token _ (TkEndDat   p2))
                             :(BzS_Expr  p x)
                             :(BzS_Token _ (TkStartDat p0)):_)       = (ParseErr p "Only integers can be used to denote the size of an array."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token p1 (TkArrMod _))
                             :_)                                     = (ParseErr p1 "Unexpected array modifier (..)."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token p1 (TkLambdaSym _))
                             :_)                                     = (ParseErr p1 "Unexpected semicolon (;)."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token p1 (TkDefine _))
                             :_)                                     = (ParseErr p1 "Unexpected definition symbol (::)."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token p1 (TkReference _))
                             :_)                                     = (ParseErr p1 "Unexpected reference symbol (@)."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token p2 (TkNewline _))
                             :(BzS_FnHead p1 _ _ _)
                             :_)                                     = (ParseErr p2 "Newlines are not allowed immediated after a definition symbol (::)."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@((BzS_Token p2 (TkNewline _))
                             :(BzS_TyHead p1 _ _)
                             :_)                                     = (ParseErr p2 "Newlines are not allowed immediated after a definition symbol (::)."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@(_
                             :(BzS_FnHead p1 _ _ _)
                             :_)                                     = (ParseErr p1 "Improper definition of function or function type."):(parserErr fname nxt (n:stk))

parserErr fname (n:nxt) stk@(_
                             :(BzS_TyHead p1 _ _)
                             :_)                                     = (ParseErr p1 "Improper definition of type."):(parserErr fname nxt (n:stk))

-- | Control Logic

parserErr fname [] (s:stack)     = []

parserErr fname (s:nxt) stack    = parserErr fname nxt (s:stack)
