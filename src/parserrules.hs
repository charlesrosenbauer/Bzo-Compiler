module BzoParserRules where
import BzoParser
import BzoSyntax
import BzoTypes










testParserFail :: Parser
testParserFail = Parser (\ps -> Left [ParseErr " * Test * "] )










testParserPass :: Parser
testParserPass = Parser (\ps ->
  Right (ParserState [PI_BzSyn $ BzS_Str mockPos " * Pass * "] [] ) )










genericParseOp :: [MockParseItem] -> ([ParseItem] -> ParseItem) -> ParserOp
genericParseOp mpi xform = ParserOp (\ps ->
  case (match ps mpi) of
    Nothing -> Nothing
    Just (xs, (ParserState s i)) -> Just (ParserState ([xform $ reverse xs] ++ s) i) )










genericNotLookaheadParseOp :: [MockParseItem] -> [BzoToken] -> ([ParseItem] -> ParseItem) -> ParserOp
genericNotLookaheadParseOp mpi bzt xform = ParserOp (\ps ->
  case (matchNotLookahead ps mpi bzt) of
    Nothing -> Nothing
    Just (xs, (ParserState s i)) -> Just (ParserState ([xform $ reverse xs] ++ s) i) )









{-
parseModifiers0 :: ParserOp
parseModifiers0 = genericParseOp [mtk_StartDat, mtk_EndDat] (\psi ->
  PI_BzSyn $ BzS_ArrGenMod (spos $ piTok $ head psi) )










parseModifiers1 :: ParserOp
parseModifiers1 = genericParseOp [mtk_ArrGnrl] (\psi ->
  PI_BzSyn $ BzS_ArrGenMod (spos $ piTok $ head psi) )










parseModifiers2 :: ParserOp
parseModifiers2 = genericParseOp [mtk_StartDat, MP_Int, mtk_EndDat] (\psi ->
  PI_BzSyn $ BzS_ArrSzMod (spos $ piTok $ head psi) (sint $ piSyn $ psi !! 1) )










parseModifiers3 :: ParserOp
parseModifiers3 = genericParseOp [mtk_StartDat, MP_Expr, mtk_EndDat] (\psi ->
  case (psi !! 1) of
    (PI_BzSyn (BzS_Expr _ [BzS_Int _ i])) -> PI_BzSyn $ BzS_ArrSzMod   (spos $ piTok $ head psi) i
    (PI_BzSyn (BzS_Expr p            x )) -> PI_BzSyn $ BzS_ArrExprMod (spos $ piTok $ head psi) (BzS_Expr p x) )










parseModifiers :: Parser
parseModifiers = Parser (\ps ->
  let parseFn = [parseModifiers0, parseModifiers1, parseModifiers2, parseModifiers3]
  in case (tryParsers ps parseFn) of
    Just pst -> Right pst
    Nothing  -> Left  [] )









parseExpr0 :: ParserOp
parseExpr0 = genericParseOp [mtk_Id] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_Id (spos $ piTok $ head tks) (valId $ piTok $ head tks)] )










parseExpr1 :: ParserOp
parseExpr1 = genericParseOp [mtk_TypeId] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_TyId (spos $ piTok $ head tks) (valId $ piTok $ head tks)] )










parseExpr2 :: ParserOp
parseExpr2 = genericParseOp [mtk_Builtin] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_BId (spos $ piTok $ head tks) (valId $ piTok $ head tks)] )










parseExpr3 :: ParserOp
parseExpr3 = genericParseOp [mtk_BIType] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_BTId (spos $ piTok $ head tks) (valId $ piTok $ head tks)] )










parseExpr4 :: ParserOp
parseExpr4 = genericParseOp [mtk_Int] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_Int (spos $ piTok $ head tks) (valInt $ piTok $ head tks)] )










parseExpr5 :: ParserOp
parseExpr5 = genericParseOp [mtk_Flt] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_Flt (spos $ piTok $ head tks) (valFlt $ piTok $ head tks)] )










parseExpr6 :: ParserOp
parseExpr6 = genericParseOp [mtk_Str] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_Str (spos $ piTok $ head tks) (valStr $ piTok $ head tks)] )










parseExpr7 :: ParserOp
parseExpr7 = genericParseOp [mtk_MutId] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_MId (spos $ piTok $ head tks) (valId $ piTok $ head tks)] )










parseExpr8 :: ParserOp
parseExpr8 = genericParseOp [MP_Poly] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseExpr9 :: ParserOp
parseExpr9 = genericParseOp [MP_Cmpd] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseExpr10 :: ParserOp
parseExpr10 = genericParseOp [MP_Box] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [expr $ piSyn $ head psi] )










parseExpr11 :: ParserOp
parseExpr11 = genericParseOp [mtk_TupEmpt] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_Nil (spos $ piTok $ head tks)] )










parseExpr12 :: ParserOp
parseExpr12 = genericParseOp [MP_Name] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [BzS_Namespace (pos $ piSyn $ head psi) (sid $ piSyn $ head psi)] )










parseExpr13 :: ParserOp
parseExpr13 = genericParseOp [mtk_Wildcard] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [BzS_Wildcard (pos $ piSyn $ head psi)] )










parseExpr14 :: ParserOp
parseExpr14 = genericParseOp [MP_AGMod] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseExpr15 :: ParserOp
parseExpr15 = genericParseOp [MP_ASMod] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseExpr16 :: ParserOp
parseExpr16 = genericParseOp [MP_AXMod] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseExpr17 :: ParserOp
parseExpr17 = genericParseOp [MP_Filt] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseExprFuse :: ParserOp
parseExprFuse = genericParseOp [MP_Expr, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) ((exprs $ piSyn $ (head psi)) ++ (exprs $ piSyn $ (psi !! 1))) )










-- parse additional expression cases, combine with function and type modifiers, and filters










parseExpr :: Parser
parseExpr = Parser (\ps ->
  let parseFn = [parseExpr0,  parseExpr1,  parseExpr2,  parseExpr3,  parseExpr4,
                 parseExpr5,  parseExpr6,  parseExpr7,  parseExpr8,  parseExpr9,
                 parseExpr10, parseExpr11, parseExpr12, parseExpr13, parseExpr14,
                 parseExpr15, parseExpr16, parseExpr17, parseExprFuse,
                 parseCmpd0,  parseCmpd1,  parseCmpd2,  parseCmpd3,
                 parsePoly0,  parsePoly1,  parsePoly2,  parsePoly3 ]
      errFn   = [errCmpd,     errPoly ]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just pst,       _ ) -> Right pst
    (Nothing , Just err) -> Left [ParseErr (piErr $ head $ stack err)]
    (Nothing , Nothing ) -> Left [] )










-- parse tuples
parseCmpd0 :: ParserOp
parseCmpd0 = genericParseOp [MP_Expr, mtk_SepExpr] (\psi ->
  PI_CMPD [piSyn $ psi !! 0])










parseCmpd1 :: ParserOp
parseCmpd1 = genericParseOp [MP_Cpx, MP_Cpx] (\psi ->
  PI_CMPD $ (piSyns $ head psi) ++ (piSyns (psi !! 1)))










parseCmpd2 :: ParserOp
parseCmpd2 = genericParseOp [mtk_StartTup, MP_Cpx, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) (piSyns (psi !! 1)))










parseCmpd3 :: ParserOp
parseCmpd3 = genericParseOp [mtk_StartTup, MP_Cpx, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ((piSyns (psi !! 1)) ++ [piSyn $ psi !! 2]))










errCmpd :: ParserOp
errCmpd = genericParseOp [mtk_StartTup, MP_Cpx, MP_Plx] (\psi -> PI_Err "Invalid form of Compound-form Tuple")










errPoly :: ParserOp
errPoly = genericParseOp [mtk_StartTup, MP_Plx, MP_Cpx] (\psi -> PI_Err "Invalid form of Polymorph-form Tuple")










parsePoly0 :: ParserOp
parsePoly0 = genericParseOp [MP_Expr, mtk_SepPoly] (\psi ->
  PI_POLY [piSyn $ psi !! 0])










parsePoly1 :: ParserOp
parsePoly1 = genericParseOp [MP_Plx, MP_Plx] (\psi ->
  PI_POLY $ (piSyns $ head psi) ++ (piSyns (psi !! 1)))










parsePoly2 :: ParserOp
parsePoly2 = genericParseOp [mtk_StartTup, MP_Plx, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) (piSyns (psi !! 1)))










parsePoly3 :: ParserOp
parsePoly3 = genericParseOp [mtk_StartTup, MP_Plx, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ((piSyns (psi !! 1)) ++ [piSyn $ psi !! 2]))










simplify0 :: ParserOp
simplify0 = genericParseOp [mtk_StartTup, mtk_Newline] (\psi ->
  PI_Token $ TkStartTup (spos $ piTok $ head psi))










simplify1 :: ParserOp
simplify1 = genericParseOp [mtk_Newline, mtk_EndTup] (\psi ->
  PI_Token $ TkEndTup (spos $ piTok $ psi !! 1))










simplify2 :: ParserOp
simplify2 = genericParseOp [mtk_EndTup, mtk_Newline] (\psi ->
  PI_Token $ TkEndTup (spos $ piTok $ head psi))










simplify3 :: ParserOp
simplify3 = genericParseOp [MP_Cpx, mtk_Newline] (\psi -> (head psi))










simplify4 :: ParserOp
simplify4 = genericParseOp [MP_Plx, mtk_Newline] (\psi -> (head psi))










simplify :: Parser
simplify = Parser (\ps ->
  let parseFn = [simplify0, simplify1, simplify2, simplify3, simplify4]
  in case (tryParsers ps parseFn) of
    Just pst -> Right pst
    Nothing  -> Left []   )










-- parse lambdas and function-related syntax
parseFnType :: ParserOp
parseFnType = genericParseOp [MP_Typ, mtk_FnSym, MP_Typ] (\psi ->
  let (p, t0) = case head psi of
        (PI_BzSyn (BzS_Box  ps  x)) -> (pos x, x)
        (PI_BzSyn (BzS_Cmpd ps xs)) -> (ps, (BzS_Cmpd ps xs))
        (PI_BzSyn (BzS_Poly ps xs)) -> (ps, (BzS_Poly ps xs))
        (PI_BzSyn (BzS_TyId ps  i)) -> (ps, (BzS_TyId ps  i))
        (PI_BzSyn (BzS_Expr p' [BzS_Box  ps  x])) -> (pos x, x)
        (PI_BzSyn (BzS_Expr p' [BzS_Cmpd ps xs])) -> (ps, (BzS_Cmpd ps xs))
        (PI_BzSyn (BzS_Expr p' [BzS_Poly ps xs])) -> (ps, (BzS_Poly ps xs))
        (PI_BzSyn (BzS_Expr p' [BzS_TyId ps  i])) -> (ps, (BzS_TyId ps  i))
      t1      = case (psi !! 2) of
        (PI_BzSyn (BzS_Box  ps  x)) -> x
        (PI_BzSyn (BzS_Cmpd ps xs)) -> (BzS_Cmpd ps xs)
        (PI_BzSyn (BzS_Poly ps xs)) -> (BzS_Poly ps xs)
        (PI_BzSyn (BzS_TyId ps  i)) -> (BzS_TyId ps  i)
        (PI_BzSyn (BzS_Expr p' [BzS_Box  ps  x])) -> x
        (PI_BzSyn (BzS_Expr p' [BzS_Cmpd ps xs])) -> (BzS_Cmpd ps xs)
        (PI_BzSyn (BzS_Expr p' [BzS_Poly ps xs])) -> (BzS_Poly ps xs)
        (PI_BzSyn (BzS_Expr p' [BzS_TyId ps  i])) -> (BzS_TyId ps  i)
  in PI_BzSyn $ BzS_FnTy p t0 t1 )










parseFnTyErr :: ParserOp
parseFnTyErr = genericParseOp [MP_Parse, mtk_FnSym, MP_Parse] (\psi ->
  PI_Err $ "Invalid Parameters to Function Type Expression : " ++ (show $ psi !! 2) )










parseFnTy :: Parser
parseFnTy = Parser (\ps ->
  case (tryParsers ps [parseFnType], tryParsers ps [parseFnTyErr]) of
    (Just pst,        _ ) -> Right pst
    (Nothing , Just errs) -> Left  [ParseErr (piErr $ head $ stack errs)]
    (Nothing , Nothing  ) -> Left  [])










parseLambda0 :: ParserOp
parseLambda0 = genericParseOp [mtk_LamdaSym, MP_Box, mtk_Define] (\psi ->
  PI_LHead (spos $ piTok $ head psi) (piSyn $ psi !! 1) )










parseLambda1 :: ParserOp
parseLambda1 = ParserOp (\ps ->
  let x = [mtk_LamdaSym, MP_Expr, mtk_Define]
  in case (match ps x) of
    Nothing                      -> Nothing
    Just (xs, (ParserState s i)) ->
      let t0p = spos  $ piTok (xs !! 0)
          t1v = exprs $ piSyn (xs !! 1)
          pm  = PI_LHead t0p (t1v !! 0)
      in case t1v of
        [x] -> Just (ParserState ([pm] ++ s) i)
        xs  -> Nothing )










parseLambda2 :: ParserOp
parseLambda2 = genericParseOp [MP_LHead, MP_Expr, mtk_Newline] (\psi ->
  PI_BzSyn $ BzS_Lambda (piPos $ head psi) (piSyn $ head psi) (piSyn $ psi !! 1) )










parseLambdaErr :: ParserOp
parseLambdaErr = ParserOp (\ps ->
  let x = [mtk_LamdaSym, MP_Expr, mtk_Define]
  in case (match ps x) of
    Nothing                      -> Nothing
    Just (xs, (ParserState s i)) ->
      case (exprs $ piSyn (xs !! 1)) of
        [x] -> Nothing
        xs  -> Just (ParserState ([PI_Err "Invalid Parameters to Lambda"]) i) )










parseLambda :: Parser
parseLambda = Parser (\ps ->
  let parseFn = [parseLambda0, parseLambda1, parseLambda2]
      errFn   = [parseLambdaErr]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just pst,        _ ) -> Right pst
    (Nothing , Just errs) -> Left  [ParseErr (piErr $ head $ stack errs)]
    (Nothing , Nothing  ) -> Left  [] )










-- parse filters
parseFilter0 :: ParserOp
parseFilter0 = genericParseOp [mtk_FilterSym, MP_TId] (\psi ->
  PI_Fltr $ piSyn (psi !! 1) )










parseFilter1 :: ParserOp
parseFilter1 = genericParseOp [mtk_FilterSym, MP_Typ] (\psi ->
  PI_Fltr $ case (psi !! 1) of
    (PI_BzSyn (BzS_Box  ps  x)) -> x
    (PI_BzSyn (BzS_Cmpd ps xs)) -> (BzS_Cmpd ps xs)
    (PI_BzSyn (BzS_Poly ps xs)) -> (BzS_Poly ps xs)
    (PI_BzSyn (BzS_TyId ps  i)) -> (BzS_TyId ps  i) )









-- parse do-blocks










-- parse records










-- parse box
parseBox0 :: ParserOp
parseBox0 = genericParseOp [mtk_StartTup, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Box  (spos $ piTok $ head psi) (piSyn (psi !! 1)) )










parseBox1 :: ParserOp
parseBox1 = genericParseOp [mtk_StartTup, MP_Box, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Box (spos $ piTok $ head psi) (piSyn (psi !! 1)) )










parseBox2 :: ParserOp
parseBox2 = genericParseOp [mtk_StartTup, MP_FnTy, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Box (spos $ piTok $ head psi) (piSyn (psi !! 1)) )










parseBox3 :: ParserOp
parseBox3 = genericParseOp [mtk_StartTup, MP_Lambda, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Box (spos $ piTok $ head psi) (piSyn (psi !! 1)) )










parseBox :: Parser
parseBox = Parser (\ps ->
  let parseFn = [parseBox0, parseBox1, parseBox2, parseBox3]
  in case (tryParsers ps parseFn) of
    Just pst -> Right pst
    Nothing  -> Left [] )










-- parse call/definition types
parseCall0 :: ParserOp
parseCall0 = genericParseOp [MP_Id, mtk_Define, MP_FnTy] (\psi ->
  PI_BzSyn $ BzS_FnTypeDef (pos $ piSyn $ head psi) (sid $ piSyn $ head psi) (tyIn $ piSyn $ psi !! 2) (tyEx $ piSyn $ psi !! 2) )










-- parse namespace
parseName0 :: ParserOp
parseName0 = genericParseOp [mtk_Reference, mtk_TypeId] (\psi ->
  PI_BzSyn $ BzS_Namespace (spos $ piTok $ head psi) (valId $ piTok $ (psi !! 1)) )










parseNameErr :: ParserOp
parseNameErr = genericParseOp [mtk_Reference, MP_Any] (\psi -> PI_Err "Invalid Namespace Identifier")










parseName :: Parser
parseName = Parser (\ps ->
  let parseFn = [parseName0]
      errFn   = [parseNameErr]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just pst,       _ ) -> Right pst
    (Nothing , Just pst) -> Left [ParseErr (piErr $ head $ stack pst)]
    (Nothing , Nothing ) -> Left []   )










parsePrimitives0 :: ParserOp
parsePrimitives0 = genericParseOp [mtk_StartTup, mtk_EndTup] (\psi ->
  PI_Token $ TkTupEmpt (spos $ piTok $ head psi) )










parsePrimitives1 :: ParserOp
parsePrimitives1 = genericParseOp [mtk_StartDat, mtk_EndDat] (\psi ->
  PI_Token $ TkArrGnrl (spos $ piTok $ head psi) )










parsePrimitives :: Parser
parsePrimitives = Parser (\ps ->
  let parseFn = [parsePrimitives0, parsePrimitives1]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left []   )
-}










parsePrimitive0 :: ParserOp
parsePrimitive0 = genericParseOp [mtk_Id] (\tk ->
  PI_BzSyn $ BzS_Id (spos $ piTok $ head tk) (valId $ piTok $ head tk))










parsePrimitive1 :: ParserOp
parsePrimitive1 = genericParseOp [mtk_MutId] (\tk ->
  PI_BzSyn $ BzS_MId (spos $ piTok $ head tk) (valId $ piTok $ head tk))










parsePrimitive2 :: ParserOp
parsePrimitive2 = genericParseOp [mtk_TypeId] (\tk ->
  PI_BzSyn $ BzS_TyId (spos $ piTok $ head tk) (valId $ piTok $ head tk))










parsePrimitive3 :: ParserOp
parsePrimitive3 = genericParseOp [mtk_Int] (\tk ->
  PI_BzSyn $ BzS_Int (spos $ piTok $ head tk) (valInt $ piTok $ head tk))










parsePrimitive4 :: ParserOp
parsePrimitive4 = genericParseOp [mtk_Flt] (\tk ->
  PI_BzSyn $ BzS_Flt (spos $ piTok $ head tk) (valFlt $ piTok $ head tk))










parsePrimitive5 :: ParserOp
parsePrimitive5 = genericParseOp [mtk_Str] (\tk ->
  PI_BzSyn $ BzS_Str (spos $ piTok $ head tk) (valStr $ piTok $ head tk))










parsePrimitive6 :: ParserOp
parsePrimitive6 = genericParseOp [mtk_Builtin] (\tk ->
  PI_BzSyn $ BzS_BId (spos $ piTok $ head tk) (valId $ piTok $ head tk))










parsePrimitive7 :: ParserOp
parsePrimitive7 = genericParseOp [mtk_BIType] (\tk ->
  PI_BzSyn $ BzS_BId (spos $ piTok $ head tk) (valId $ piTok $ head tk))










parsePrimitive8 :: ParserOp
parsePrimitive8 = genericParseOp [mtk_Wildcard] (\tk ->
  PI_BzSyn $ BzS_Wildcard (spos $ piTok $ head tk))










parsePrimitive9 :: ParserOp
parsePrimitive9 = genericParseOp [mtk_TupEmpt] (\tk ->
  PI_BzSyn $ BzS_Nil (spos $ piTok $ head tk))










parsePrimitive10 :: ParserOp
parsePrimitive10 = genericParseOp [mtk_ArrMod] (\tk ->
  PI_BzSyn $ BzS_MapMod (spos $ piTok $ head tk))










parsePrimitives :: Parser
parsePrimitives = Parser (\ps ->
  let parseFn = [parsePrimitive0,  parsePrimitive1,  parsePrimitive2,  parsePrimitive3,
                 parsePrimitive4,  parsePrimitive5,  parsePrimitive6,  parsePrimitive7,
                 parsePrimitive8,  parsePrimitive9,  parsePrimitive10]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left []  )










parseCompound0 :: ParserOp
parseCompound0 = genericParseOp [MP_Item, mtk_SepExpr] (\psi ->
  PI_CPX $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi])










parseCompound1 :: ParserOp
parseCompound1 = genericParseOp [MP_Item, MP_Cpx] (\psi ->
  PI_CPX $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi] ++ (exprs $ piSyn $ (psi !! 1))) )










parseCompound2 :: ParserOp
parseCompound2 = genericParseOp [MP_Cpx, MP_Cpx] (\psi ->
  PI_CPXS $ [piSyn $ head psi] ++ [piSyn $ (psi !! 1)] )










parseCompound3 :: ParserOp
parseCompound3 = genericParseOp [MP_Cpxs, MP_Cpx] (\psi ->
  PI_CPXS $ [piSyn $ psi !! 1] ++ (piSyns $ (head psi)) )










parseCompound4 :: ParserOp
parseCompound4 = genericParseOp [mtk_StartTup, MP_Cpx, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) [piSyn $ psi !! 1] )










parseCompound5 :: ParserOp
parseCompound5 = genericParseOp [mtk_StartTup, MP_Cpxs, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) (piSyns $ psi !! 1) )










parseCompound :: Parser
parseCompound = Parser (\ps ->
  let parseFn = [parseCompound0, parseCompound1, parseCompound2,
                 parseCompound3, parseCompound4, parseCompound5]
  in case tryParsers ps parseFn of
    Just ps -> Right ps
    Nothing -> Left [] )










parsePolymorph0 :: ParserOp
parsePolymorph0 = genericParseOp [MP_Item, mtk_SepPoly] (\psi ->
  PI_PLX $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi])










parsePolymorph1 :: ParserOp
parsePolymorph1 = genericParseOp [MP_Item, MP_Plx] (\psi ->
  PI_PLX $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi] ++ (exprs $ piSyn $ (psi !! 1))) )










parsePolymorph2 :: ParserOp
parsePolymorph2 = genericParseOp [MP_Plx, MP_Plx] (\psi ->
  PI_PLXS $ [piSyn $ head psi] ++ [piSyn $ (psi !! 1)] )










parsePolymorph3 :: ParserOp
parsePolymorph3 = genericParseOp [MP_Plxs, MP_Plx] (\psi ->
  PI_PLXS $ [piSyn $ psi !! 1] ++ (piSyns $ (head psi)) )










parsePolymorph4 :: ParserOp
parsePolymorph4 = genericParseOp [mtk_StartTup, MP_Plx, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) [piSyn $ psi !! 1] )










parsePolymorph5 :: ParserOp
parsePolymorph5 = genericParseOp [mtk_StartTup, MP_Plxs, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) (piSyns $ psi !! 1) )










parsePolymorph :: Parser
parsePolymorph = Parser (\ps ->
  let parseFn = [parsePolymorph0, parsePolymorph1, parsePolymorph2,
                 parsePolymorph3, parsePolymorph4, parsePolymorph5]
  in case tryParsers ps parseFn of
    Just ps -> Right ps
    Nothing -> Left [] )










parseCalls :: Parser
parseCalls = Parser (\ps ->
  case (runParsers ps [parsePrimitives, parseCompound, parsePolymorph]) of
    Left []   -> Left []
    Left err  -> Left err
    Right ps' -> Right ps' )
