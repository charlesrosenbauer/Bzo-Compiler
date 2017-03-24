module BzoParserRules where
import BzoParser
import BzoSyntax
import BzoTypes










testParserFail :: Parser
testParserFail = Parser (\ps -> Left [ParseErr " * Test * "] )










testParserPass :: Parser
testParserPass = Parser (\ps ->
  Right (ParserState [PI_BzSyn $ BzS_Str mockPos " * Pass * "] [] ) )










parseModifiers0 :: ParserOp
parseModifiers0 = ParserOp (\ps ->
  case (match ps [mtk_ArrGnrl]) of
    Nothing     -> Nothing
    Just ([x], (ParserState s i)) ->
      let tp = spos $ piTok x
          pm = PI_BzSyn $ BzS_Modifiers tp [Arry]
      in Just (ParserState ([pm] ++ s) i) )










parseModifiers1 :: ParserOp
parseModifiers1 = ParserOp (\ps ->
  let x = [mtk_StartDat, mtk_Int, mtk_EndDat]
  in case (match ps x) of
    Nothing                      -> Nothing
    Just (xs, (ParserState s i)) ->
      let t0p = spos   $ piTok (xs !! 0)
          t1v = valInt $ piTok (xs !! 1)
          pm  = PI_BzSyn $ BzS_Modifiers t0p [ArSz t1v]
      in Just (ParserState ([pm] ++ s) i) )










parseModifiers2 :: ParserOp
parseModifiers2 = ParserOp (\ps ->
  let x = [mtk_StartDat, mtk_Id, mtk_EndDat]
  in case (match ps x) of
    Nothing     -> Nothing
    Just (xs, (ParserState s i)) ->
      let t0p = spos  $ piTok (xs !! 0)
          t1v = valId $ piTok (xs !! 1)
          pm  = PI_BzSyn $ BzS_Modifiers t0p [ArVr t1v]
      in Just (ParserState ([pm] ++ s) i) )










parseModifiers3 :: ParserOp
parseModifiers3 = ParserOp (\ps ->
  let x = [mtk_StartDat, mtk_EndDat]
  in case (match ps x) of
    Nothing     -> Nothing
    Just (xs, (ParserState s i)) ->
      let tp = spos  $ piTok (xs !! 0)
          pm = PI_BzSyn $ BzS_Modifiers tp [Arry]
      in Just (ParserState ([pm] ++ s) i) )










parseModifiers4 :: ParserOp
parseModifiers4 = ParserOp (\ps ->
  let x = [MP_Mods, MP_Mods]
  in case (match ps x) of
    Nothing -> Nothing
    Just (xs, (ParserState s i)) ->
      let t0p = pos  $ piSyn (xs !! 0)
          t0v = mods $ piSyn (xs !! 0)
          t1v = mods $ piSyn (xs !! 1)
          ms  = PI_BzSyn $ BzS_Modifiers t0p (t1v ++ t0v)
      in Just (ParserState ([ms] ++ s) i) )










-- | If signs of a modifier are detected, but no modifer is parsed, flag an error!
parseModifiers :: Parser
parseModifiers = Parser (\ps ->
  let parseFn = [parseModifiers0, parseModifiers1, parseModifiers2, parseModifiers3, parseModifiers4]
  in case (tryParsers ps parseFn) of
    Just pst -> Right pst
    Nothing  ->
      case ps of
        (ParserState ((PI_Token (TkEndDat (BzoPos l c f))) : (PI_Token _) : ss) i) ->
          Left [ParseErr ("Expected Valid Array Modifier at " ++ (show l) ++ ":" ++ (show c) ++ " in " ++ f)]
        --(ParserState s []) -> Left [ParseErr $ show s]
        _ -> Left []   )










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
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseExpr11 :: ParserOp
parseExpr11 = genericParseOp [mtk_TupEmpt] (\tks ->
  PI_BzSyn $ BzS_Expr (spos $ piTok $ head tks) [BzS_Undefined (spos $ piTok $ head tks)] )










parseExprFuse :: ParserOp
parseExprFuse = genericParseOp [MP_Expr, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) ((exprs $ piSyn $ (head psi)) ++ (exprs $ piSyn $ (psi !! 1))) )










-- parse additional expression cases, combine with function and type modifiers, and filters










parseExpr :: Parser
parseExpr = Parser (\ps ->
  let parseFn = [parseExpr0,  parseExpr1,  parseExpr2,  parseExpr3,  parseExpr4,
                 parseExpr5,  parseExpr6,  parseExpr7,  parseExpr8,  parseExpr9,
                 parseExpr10, parseExpr11, parseExprFuse,
                 parseCmpd0,  parseCmpd1,  parseCmpd2,  parseCmpd3,
                 parsePoly0,  parsePoly1,  parsePoly2,  parsePoly3 ]
  in case (tryParsers ps parseFn) of
    Just pst -> Right pst
    Nothing  -> Left []   )










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
        (PI_Box                x  ) -> (pos x, x)
        (PI_BzSyn (BzS_Cmpd ps xs)) -> (ps, (BzS_Cmpd ps xs))
        (PI_BzSyn (BzS_Poly ps xs)) -> (ps, (BzS_Poly ps xs))
        (PI_BzSyn (BzS_TyId ps  i)) -> (ps, (BzS_TyId ps  i))
      t1      = case head psi of
        (PI_Box                x  ) -> x
        (PI_BzSyn (BzS_Cmpd ps xs)) -> (BzS_Cmpd ps xs)
        (PI_BzSyn (BzS_Poly ps xs)) -> (BzS_Poly ps xs)
        (PI_BzSyn (BzS_TyId ps  i)) -> (BzS_TyId ps  i)
  in PI_BzSyn $ BzS_FnTy p t0 t1 )










-- parse filters










-- parse do-blocks










-- parse records










-- parse call/definition types










parseCalls :: Parser
parseCalls = Parser (\ps ->
  case (runParsers ps [parseExpr, parseModifiers, simplify]) of  -- | Temporary!
    Left []   -> Left []
    Left err  -> Left err
    Right ps' -> Right ps' )
