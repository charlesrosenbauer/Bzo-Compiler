module BzoParserRules where
import BzoParser
import BzoSyntax
import BzoTypes










parseModifiers0 :: ParserOp
parseModifiers0 = ParserOp (\ps ->
  case (match ps [(MP_Tk $ TkArrGnrl mockPos)]) of
    Nothing     -> Nothing
    Just ([x], (ParserState s i)) ->
      let tp = spos $ piTok x
          pm = PI_BzSyn $ BzS_Modifiers tp [Arry]
      in Just (ParserState ([pm] ++ s) i) )










parseModifiers1 :: ParserOp
parseModifiers1 = ParserOp (\ps ->
  let x = [(MP_Tk $ TkStartDat mockPos), (MP_Tk $ TkInt mockPos 0),
           (MP_Tk $ TkEndDat mockPos)]
  in case (match ps x) of
    Nothing                      -> Nothing
    Just (xs, (ParserState s i)) ->
      let t0p = spos $ piTok (xs !! 0)
          t1v = valInt $ piTok (xs !! 1)
          pm  = PI_BzSyn $ BzS_Modifiers t0p [ArSz t1v]
      in Just (ParserState ([pm] ++ s) i) )










parseModifiers2 :: ParserOp
parseModifiers2 = ParserOp (\ps ->
  let x = [(MP_Tk $ TkStartDat mockPos), (MP_Tk $ TkId mockPos ""),
           (MP_Tk $ TkEndDat mockPos)]
  in case (match ps x) of
    Nothing     -> Nothing
    Just (xs, (ParserState s i)) ->
      let t0p = spos $ piTok (xs !! 0)
          t1v = valId $ piTok (xs !! 1)
          pm  = PI_BzSyn $ BzS_Modifiers t0p [ArVr t1v]
      in Just (ParserState ([pm] ++ s) i) )










parseCalls :: Parser
parseCalls = Parser (\ps ->
  case ps of
    (ParserState s i) -> Left [ParseErr "TestSuccess"] )
