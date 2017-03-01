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
    Just (xs, (ParserState s i)) -> Just (ParserState ([xform xs] ++ s) i) )









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








parseCalls :: Parser
parseCalls = Parser (\ps ->
  case (runParsers ps [parseModifiers]) of  -- | Temporary!
    Left []   -> Left []
    Left err  -> Left err
    Right ps' -> Right ps' )
