module BzoParserRules where
import BzoParser
import BzoSyntax
import BzoTypes










testParserFail :: Parser
testParserFail = Parser (\(ParserState f p s i) -> Left [ParseErr p " * Test * "] )










testParserPass :: Parser
testParserPass = Parser (\ps ->
  Right (ParserState "Test" (BzoPos 0 0 "Test") [PI_BzSyn $ BzS_Str mockPos " * Pass * "] [] ) )










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










parsePrimitive11 :: ParserOp
parsePrimitive11 = genericParseOp [mtk_TyVar] (\tk ->
  PI_BzSyn $ BzS_TyVar (spos $ piTok $ head tk) (valId $ piTok $ head tk))










parsePrimitives :: Parser
parsePrimitives = Parser (\ps ->
  let parseFn = [parsePrimitive0,  parsePrimitive1,  parsePrimitive2,  parsePrimitive3,
                 parsePrimitive4,  parsePrimitive5,  parsePrimitive6,  parsePrimitive7,
                 parsePrimitive8,  parsePrimitive9,  parsePrimitive10, parsePrimitive11]
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










parseCompound6 :: ParserOp
parseCompound6 = genericParseOp [mtk_StartTup, MP_Cpxs, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 2]) )










parseCompound7 :: ParserOp
parseCompound7 = genericParseOp [mtk_StartTup, MP_Cpx, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 2]) )










parseCompound8 :: ParserOp
parseCompound8 = genericParseOp [MP_Cpxs, mtk_Newline, MP_Cpx] (\psi ->
  PI_CPXS $ [piSyn $ psi !! 2] ++ (piSyns $ (head psi)) )










parseCompound9 :: ParserOp
parseCompound9 = genericParseOp [MP_Cpx, mtk_Newline, MP_Cpx] (\psi ->
  PI_CPXS $ [piSyn $ head psi] ++ [piSyn $ (psi !! 2)] )










parseCompound10 :: ParserOp
parseCompound10 = genericParseOp [mtk_StartTup, MP_Cpx, mtk_Newline, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 3]) )










parseCompound11 :: ParserOp
parseCompound11 = genericParseOp [mtk_StartTup, MP_Cpxs, mtk_Newline, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 3]) )










parseCompound12 :: ParserOp
parseCompound12 = genericParseOp [mtk_StartTup, MP_Cpxs, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 2]) )










parseCompound13 :: ParserOp
parseCompound13 = genericParseOp [mtk_StartTup, MP_Cpx, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 2]) )










parseCompound14 :: ParserOp
parseCompound14 = genericParseOp [mtk_StartTup, MP_Cpx, mtk_Newline, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 3]) )










parseCompound15 :: ParserOp
parseCompound15 = genericParseOp [mtk_StartTup, MP_Cpxs, mtk_Newline, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 3]) )










parseCompoundErr0 :: ParserOp
parseCompoundErr0 = genericParseOp [MP_Cpx, MP_Plx] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr1 :: ParserOp
parseCompoundErr1 = genericParseOp [MP_Cpxs, MP_Plx] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr2 :: ParserOp
parseCompoundErr2 = genericParseOp [MP_Cpx, MP_Plxs] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr3 :: ParserOp
parseCompoundErr3 = genericParseOp [MP_Cpxs, MP_Plxs] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr4 :: ParserOp
parseCompoundErr4 = genericParseOp [MP_Cpx, mtk_Newline, MP_Plx] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr5 :: ParserOp
parseCompoundErr5 = genericParseOp [MP_Cpxs, mtk_Newline, MP_Plx] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr6 :: ParserOp
parseCompoundErr6 = genericParseOp [MP_Cpx, mtk_Newline, MP_Plxs] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr7 :: ParserOp
parseCompoundErr7 = genericParseOp [MP_Cpxs, mtk_Newline, MP_Plxs] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parseCompound :: Parser
parseCompound = Parser (\ps ->
  let parseFn = [parseCompound0,  parseCompound1,  parseCompound2,
                 parseCompound3,  parseCompound4,  parseCompound5,
                 parseCompound6,  parseCompound7,  parseCompound8,
                 parseCompound9,  parseCompound10, parseCompound11,
                 parseCompound12, parseCompound13, parseCompound14,
                 parseCompound15]
      errFn = [parseCompoundErr0, parseCompoundErr1, parseCompoundErr2, parseCompoundErr3,
               parseCompoundErr4, parseCompoundErr5, parseCompoundErr6, parseCompoundErr7]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just ps,      _ ) -> Right ps
    (Nothing, Nothing) -> Left []
    (Nothing, Just er) -> Left [piErr $ head $ stack er])










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










parsePolymorph6 :: ParserOp
parsePolymorph6 = genericParseOp [mtk_StartTup, MP_Plxs, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 2]) )










parsePolymorph7 :: ParserOp
parsePolymorph7 = genericParseOp [mtk_StartTup, MP_Plx, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 2]) )










parsePolymorph8 :: ParserOp
parsePolymorph8 = genericParseOp [MP_Plxs, mtk_Newline, MP_Plx] (\psi ->
  PI_PLXS $ [piSyn $ psi !! 2] ++ (piSyns $ (head psi)) )










parsePolymorph9 :: ParserOp
parsePolymorph9 = genericParseOp [MP_Plx, mtk_Newline, MP_Plx] (\psi ->
  PI_PLXS $ [piSyn $ head psi] ++ [piSyn $ (psi !! 2)] )










parsePolymorph10 :: ParserOp
parsePolymorph10 = genericParseOp [mtk_StartTup, MP_Plx, mtk_Newline, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 3]) )










parsePolymorph11 :: ParserOp
parsePolymorph11 = genericParseOp [mtk_StartTup, MP_Plxs, mtk_Newline, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 3]) )










parsePolymorph12 :: ParserOp
parsePolymorph12 = genericParseOp [mtk_StartTup, MP_Plxs, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 2]) )










parsePolymorph13 :: ParserOp
parsePolymorph13 = genericParseOp [mtk_StartTup, MP_Plx, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 2]) )










parsePolymorph14 :: ParserOp
parsePolymorph14 = genericParseOp [mtk_StartTup, MP_Plx, mtk_Newline, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 3]) )










parsePolymorph15 :: ParserOp
parsePolymorph15 = genericParseOp [mtk_StartTup, MP_Plxs, mtk_Newline, MP_Expr, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Poly (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 3]) )










parsePolymorphErr0 :: ParserOp
parsePolymorphErr0 = genericParseOp [MP_Plx, MP_Cpx] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr1 :: ParserOp
parsePolymorphErr1 = genericParseOp [MP_Plxs, MP_Cpx] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr2 :: ParserOp
parsePolymorphErr2 = genericParseOp [MP_Plx, MP_Cpxs] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr3 :: ParserOp
parsePolymorphErr3 = genericParseOp [MP_Plxs, MP_Cpxs] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr4 :: ParserOp
parsePolymorphErr4 = genericParseOp [MP_Plx, mtk_Newline, MP_Cpx] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr5 :: ParserOp
parsePolymorphErr5 = genericParseOp [MP_Plxs, mtk_Newline, MP_Cpx] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr6 :: ParserOp
parsePolymorphErr6 = genericParseOp [MP_Plx, mtk_Newline, MP_Cpxs] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr7 :: ParserOp
parsePolymorphErr7 = genericParseOp [MP_Plxs, mtk_Newline, MP_Cpxs] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid combination of polymorphic and compound expressions.")










parsePolymorph :: Parser
parsePolymorph = Parser (\ps ->
  let parseFn = [parsePolymorph0,  parsePolymorph1,  parsePolymorph2,
                 parsePolymorph3,  parsePolymorph4,  parsePolymorph5,
                 parsePolymorph6,  parsePolymorph7,  parsePolymorph8,
                 parsePolymorph9,  parsePolymorph10, parsePolymorph11,
                 parsePolymorph12, parsePolymorph13, parsePolymorph14,
                 parsePolymorph15]
      errFn   = [parsePolymorphErr0, parsePolymorphErr1, parsePolymorphErr2, parsePolymorphErr3,
                 parsePolymorphErr4, parsePolymorphErr5, parsePolymorphErr6, parsePolymorphErr7]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just ps,      _ ) -> Right ps
    (Nothing, Nothing) -> Left []
    (Nothing, Just er) -> Left [piErr $ head $ stack er])










parseTupleEtc0 :: ParserOp
parseTupleEtc0 = genericParseOp [MP_Item, mtk_EndTup] (\psi ->
  PI_TX $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseTupleEtc1 :: ParserOp
parseTupleEtc1 = genericParseOp [MP_Item, MP_Tx] (\psi ->
  PI_TX $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi] ++ (exprs $ piSyn $ psi !! 1)) )










parseTupleEtc2 :: ParserOp
parseTupleEtc2 = genericParseOp [mtk_StartTup, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Box (spos $ piTok $ head psi) (piSyn $ psi !! 1) )










parseTupleEtc3 :: ParserOp
parseTupleEtc3 = genericParseOp [mtk_StartTup, MP_FnTy, mtk_EndTup] (\psi ->
  PI_BzSyn $ BzS_Box (spos $ piTok $ head psi) (piSyn $ psi !! 1) )










parseTupleEtc4 :: ParserOp
parseTupleEtc4 = genericParseOp [MP_Item, mtk_Newline, mtk_EndTup] (\psi ->
  PI_TX $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseTupleEtc :: Parser
parseTupleEtc = Parser (\ps ->
  let parseFn = [parseTupleEtc0, parseTupleEtc1, parseTupleEtc2, parseTupleEtc3,
                 parseTupleEtc4]
  in case tryParsers ps parseFn of
    Just ps -> Right ps
    Nothing -> Left  [] )










parseSimplify0 :: ParserOp
parseSimplify0 = genericParseOp [mtk_Newline, mtk_EndTup] (\psi ->
  PI_Token $ TkEndTup (spos $ piTok $ psi !! 1) )










parseSimplify1 :: ParserOp
parseSimplify1 = genericParseOp [mtk_StartTup, mtk_Newline] (\psi ->
  PI_Token $ TkStartTup (spos $ piTok $ psi !! 0) )










parseSimplify2 :: ParserOp
parseSimplify2 = genericParseOp [mtk_StartTup, mtk_EndTup] (\psi ->
  PI_Token $ TkTupEmpt (spos $ piTok $ psi !! 1) )










parseSimplify3 :: ParserOp
parseSimplify3 = genericParseOp [mtk_StartTup, mtk_Newline, mtk_EndTup] (\psi ->
  PI_Token $ TkTupEmpt (spos $ piTok $ psi !! 0) )










parseSimplify4 :: ParserOp
parseSimplify4 = genericParseOp [mtk_StartDat, mtk_EndDat] (\psi ->
  PI_Token $ TkArrGnrl (spos $ piTok $ psi !! 0) )










parseSimplify5 :: ParserOp
parseSimplify5 = genericParseOp [mtk_StartDat, mtk_Newline, mtk_EndDat] (\psi ->
  PI_Token $ TkArrGnrl (spos $ piTok $ psi !! 0) )










parseSimplify6 :: ParserOp
parseSimplify6 = genericParseOp [mtk_StartDat, mtk_Newline] (\psi ->
  PI_Token $ TkStartDat (spos $ piTok $ psi !! 0) )










parseSimplify7 :: ParserOp
parseSimplify7 = genericParseOp [mtk_Newline, mtk_EndDat] (\psi ->
  PI_Token $ TkEndDat (spos $ piTok $ psi !! 1) )










parseSimplify8 :: ParserOp
parseSimplify8 = genericParseOp [mtk_StartDo, mtk_Newline] (\psi ->
  PI_Token $ TkStartDo (spos $ piTok $ psi !! 0) )










parseSimplify9 :: ParserOp
parseSimplify9 = genericParseOp [mtk_Newline, mtk_EndDo] (\psi ->
  PI_Token $ TkEndDo (spos $ piTok $ psi !! 1) )










parseSimplify :: Parser
parseSimplify = Parser (\ps ->
  let parseFn = [parseSimplify0, parseSimplify1, parseSimplify2, parseSimplify3,
                 parseSimplify4, parseSimplify5, parseSimplify6, parseSimplify7,
                 parseSimplify8, parseSimplify9 ]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left [] )










parseExpr0 :: ParserOp
parseExpr0 = genericParseOp [MP_Item, mtk_Newline] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseExpr1 :: ParserOp
parseExpr1 = genericParseOp [MP_Item, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi] ++ (exprs $ piSyn $ psi !! 1)) )










parseExpr :: Parser
parseExpr = Parser (\ps ->
  let parseFn = [parseExpr0, parseExpr1]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left [] )










parseFilter :: ParserOp
parseFilter = genericParseOp [mtk_FilterSym, MP_Item] (\psi ->
  PI_BzSyn $ BzS_Filter (spos $ piTok $ head psi) (piSyn $ psi !! 1) )










parseName :: ParserOp
parseName = genericParseOp [mtk_Reference, MP_TId] (\psi ->
  PI_BzSyn $ BzS_Namespace (spos $ piTok $ head psi) (sid $ piSyn $ psi !! 1) )










parseCurry :: ParserOp
parseCurry = genericParseOp [MP_Item, mtk_CurrySym] (\psi ->
  PI_BzSyn $ BzS_Curry (pos $ piSyn $ head psi) (piSyn $ head psi))










parseNameErr :: ParserOp
parseNameErr = genericParseOp [mtk_Reference, MP_Any] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid Namespace Identifier" )










parseCurryErr :: ParserOp
parseCurryErr = genericParseOp [mtk_CurrySym] (\psi -> PI_Err $ ParseErr (getPIPos $ head psi) "Invalid Syntax with Curry Operator")










parseMisc :: Parser
parseMisc = Parser (\ps ->
  let parseFn = [parseFilter, parseName, parseCurry]
      errFn   = [parseNameErr, parseCurryErr]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just pst,      _ ) -> Right pst
    (Nothing , Nothing) -> Left []
    (Nothing , Just er) -> Left [piErr $ head $ stack er] )










parseTypeCall0 :: ParserOp
parseTypeCall0 = genericParseOp [MP_Tup, MP_TId, mtk_Define, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_TypDef (pos $ piSyn $ head psi) (piSyn $ head psi) (sid $ piSyn $ psi !! 1) (piSyn $ psi !! 3) )










parseTypeCall1 :: ParserOp
parseTypeCall1 = genericParseOp [MP_TId, mtk_Define, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_TypDef (pos $ piSyn $ head psi) (BzS_Undefined) (sid $ piSyn $ head psi) (piSyn $ psi !! 2) )










parseTypeCallErr :: ParserOp    -- | Not currently working. Not sure why
parseTypeCallErr = genericParseOp [MP_Any, MP_TId, mtk_Define, MP_Expr] (\psi ->
  PI_Err $ ParseErr (getPIPos $ head psi) "Invalid Parameter to Type Definition" )










parseTypeCall :: Parser
parseTypeCall = Parser (\ps ->
  let parseFn = [parseTypeCall0, parseTypeCall1]
      errFn   = [parseTypeCallErr]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just pst,      _ ) -> Right pst
    (Nothing , Nothing) -> Left []
    (Nothing , Just er) -> Left [piErr $ head $ stack er] )










parseFnCall0 :: ParserOp
parseFnCall0 = genericParseOp [MP_Tup, MP_Id, MP_Tup, mtk_Define, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_FunDef (pos $ piSyn $ head psi) (piSyn $ head psi) (sid $ piSyn $ psi !! 1) (piSyn $ psi !! 2) (piSyn $ psi !! 4) )










parseFnCall1 :: ParserOp
parseFnCall1 = genericParseOp [MP_Tup, MP_Id, mtk_Define, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_FunDef (pos $ piSyn $ head psi) (piSyn $ head psi) (sid $ piSyn $ psi !! 1) (BzS_Undefined) (piSyn $ psi !! 3) )










parseFnCall2 :: ParserOp
parseFnCall2 = genericParseOp [MP_Id, MP_Tup, mtk_Define, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_FunDef (pos $ piSyn $ head psi) (BzS_Undefined) (sid $ piSyn $ head psi) (piSyn $ psi !! 1) (piSyn $ psi !! 3) )










parseFnCall3 :: ParserOp
parseFnCall3 = genericParseOp [MP_Id, mtk_Define, MP_Expr] (\psi ->
  PI_BzSyn $ BzS_FunDef (pos $ piSyn $ head psi) (BzS_Undefined) (sid $ piSyn $ head psi) (BzS_Undefined) (piSyn $ psi !! 2) )










parseFnCall4 :: ParserOp
parseFnCall4 = genericParseOp [MP_Tup, MP_Id, MP_Tup, mtk_Define, MP_Blck, mtk_Newline] (\psi ->
  PI_BzSyn $ BzS_FunDef (pos $ piSyn $ head psi) (piSyn $ head psi) (sid $ piSyn $ psi !! 1) (piSyn $ psi !! 2) (piSyn $ psi !! 4) )










parseFnCall5 :: ParserOp
parseFnCall5 = genericParseOp [MP_Tup, MP_Id, mtk_Define, MP_Blck, mtk_Newline] (\psi ->
  PI_BzSyn $ BzS_FunDef (pos $ piSyn $ head psi) (piSyn $ head psi) (sid $ piSyn $ psi !! 1) (BzS_Undefined) (piSyn $ psi !! 3) )










parseFnCall6 :: ParserOp
parseFnCall6 = genericParseOp [MP_Id, MP_Tup, mtk_Define, MP_Blck, mtk_Newline] (\psi ->
  PI_BzSyn $ BzS_FunDef (pos $ piSyn $ head psi) (BzS_Undefined) (sid $ piSyn $ head psi) (piSyn $ psi !! 1) (piSyn $ psi !! 3) )










parseFnCall7 :: ParserOp
parseFnCall7 = genericParseOp [MP_Id, mtk_Define, MP_Blck, mtk_Newline] (\psi ->
  PI_BzSyn $ BzS_FunDef (pos $ piSyn $ head psi) (BzS_Undefined) (sid $ piSyn $ head psi) (BzS_Undefined) (piSyn $ psi !! 2) )










parseFnCall :: Parser
parseFnCall = Parser (\ps ->
  let parseFn = [parseFnCall0, parseFnCall1, parseFnCall2, parseFnCall3,
                 parseFnCall4, parseFnCall5, parseFnCall6, parseFnCall7]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left [] )










parseModifiers0 :: ParserOp
parseModifiers0 = genericParseOp [mtk_ArrGnrl] (\psi ->
  PI_BzSyn $ BzS_ArrGenMod (spos $ piTok $ head psi) )










parseModifiers1 :: ParserOp
parseModifiers1 = genericParseOp [mtk_StartDat, MP_Int, mtk_EndDat] (\psi ->
  PI_BzSyn $ BzS_ArrSzMod (spos $ piTok $ head psi) (sint $ piSyn $ psi !! 1) )










parseModifiers2 :: ParserOp
parseModifiers2 = genericParseOp [MP_Item, mtk_EndDat] (\psi ->
  PI_RX $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseModifiers3 :: ParserOp
parseModifiers3 = genericParseOp [MP_Item, MP_Rx] (\psi ->
  PI_RX $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi] ++ (exprs $ piSyn $ psi !! 1)) )










parseModifiers4 :: ParserOp
parseModifiers4 = genericParseOp [mtk_StartDat, MP_Rx] (\psi ->
  PI_BzSyn $ BzS_ArrExprMod (spos $ piTok $ head psi) (piSyn $ psi !! 1) )










parseModifiers5 :: ParserOp
parseModifiers5 = genericParseOp [MP_Mod, MP_Mod] (\psi ->
  PI_MS $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi]++[piSyn $ psi !! 1]) )










parseModifiers6 :: ParserOp
parseModifiers6 = genericParseOp [MP_Ms, MP_Mod] (\psi ->
  PI_MS $ BzS_Expr (pos $ piSyn $ head psi) ((exprs $ piSyn $ head psi) ++ [piSyn $ psi !! 1]) )










parseModifiers7 :: ParserOp
parseModifiers7 = genericParseOp [MP_Ms, MP_Item] (\psi ->
  PI_MX $ BzS_Expr (pos $ piSyn $ head psi) ((exprs $ piSyn $ head psi) ++ [piSyn $ psi !! 1]) )










parseModifiers8 :: ParserOp
parseModifiers8 = genericParseOp [MP_Mod, MP_Item] (\psi ->
  PI_MX $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi] ++ [piSyn $ psi !! 1]) )









parseModifiers :: Parser
parseModifiers = Parser (\ps ->
  let parseFn = [parseModifiers0, parseModifiers1, parseModifiers2, parseModifiers3,
                 parseModifiers4, parseModifiers5, parseModifiers6, parseModifiers7,
                 parseModifiers8 ]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left [] )










parseFnTy0 :: ParserOp
parseFnTy0 = genericParseOp [MP_Typ, mtk_FnSym, MP_Typ] (\psi ->
  PI_BzSyn $ BzS_FnTy (pos $ piSyn $ head psi) (piSyn $ head psi) (piSyn $ psi !! 2) )










parseFnTyErr :: ParserOp -- Not currently working. MP_Parse appears to trigger from Modifiers
parseFnTyErr = genericParseOp [MP_Any, mtk_FnSym, MP_Parse] (\psi ->
  PI_Err $ ParseErr (getPIPos $ head psi) "Invalid parameters to Function Type" )










parseFnTy :: Parser
parseFnTy = Parser (\ps ->
  let parseFn = [parseFnTy0]
      errFn   = [parseFnTyErr]
  in case (tryParsers ps parseFn, {-tryParsers ps errFn-}Nothing) of
    (Just pst,      _ ) -> Right pst
    (Nothing , Just er) -> Left  [piErr $ head $ stack er]
    (Nothing , Nothing) -> Left  [] )










parseFnTyDef0 :: ParserOp
parseFnTyDef0 = genericParseOp [MP_Id, mtk_Define, MP_FnTy, mtk_Newline] (\psi ->
  PI_BzSyn $ BzS_FnTypeDef (pos $ piSyn $ head psi) (sid $ piSyn $ head psi) (piSyn $ psi !! 2))










parseFnTyDef :: Parser
parseFnTyDef = Parser (\ps ->
  case tryParsers ps [parseFnTyDef0] of
    Just pst -> Right pst
    Nothing  -> Left [] )










parseLambda0 :: ParserOp
parseLambda0 = genericParseOp [mtk_LamdaSym, MP_Vr, mtk_Define, MP_Def] (\psi ->
  PI_BzSyn $ BzS_Lambda (spos $ piTok $ head psi) (piSyn $ psi !! 1) (piSyn $ psi !! 3) )










parseLambda1 :: ParserOp
parseLambda1 = genericParseOp [mtk_LamdaSym, MP_Vr, mtk_Define, MP_Cpx] (\psi ->
  PI_BzSyn $ BzS_Lambda (spos $ piTok $ head psi) (piSyn $ psi !! 1) (piSyn $ psi !! 3) )










parseLambda2 :: ParserOp
parseLambda2 = genericParseOp [mtk_StartTup, mtk_LamdaSym, MP_Vr, mtk_Define, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Box (spos $ piTok $ head psi) (BzS_Lambda (spos $ piTok $ psi !! 1) (piSyn $ psi !! 2) (piSyn $ psi !! 4)) )










parseLambda :: Parser
parseLambda = Parser (\ps ->
  let parseFn = [parseLambda0, parseLambda1, parseLambda2]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left [] )











parseBlock0 :: ParserOp
parseBlock0 = genericParseOp [mtk_StartDo, mtk_EndDo] (\psi ->
  PI_BzSyn $ BzS_Block (spos $ piTok $ head psi) [] )










parseBlock1 :: ParserOp
parseBlock1 = genericParseOp [MP_Item, mtk_EndDo] (\psi ->
  PI_BKX $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseBlock2 :: ParserOp
parseBlock2 = genericParseOp [MP_Item, MP_Bkx] (\psi ->
  PI_BKX $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi] ++ (exprs $ piSyn $ psi !! 1)) )










parseBlock3 :: ParserOp
parseBlock3 = genericParseOp [MP_Expr, MP_Expr] (\psi ->
  PI_Exs $ [piSyn $ head psi] ++ [piSyn $ psi !! 1] )










parseBlock4 :: ParserOp
parseBlock4 = genericParseOp [MP_Exs, MP_Expr] (\psi ->
  PI_Exs $ (piSyns $ head psi) ++ [piSyn $ psi !! 1] )










parseBlock5 :: ParserOp
parseBlock5 = genericParseOp [mtk_StartDo, MP_Expr, mtk_EndDo] (\psi ->
  PI_BzSyn $ BzS_Block (spos $ piTok $ head psi) [piSyn $ psi !! 1] )










parseBlock6 :: ParserOp
parseBlock6 = genericParseOp [mtk_StartDo, MP_Exs, mtk_EndDo] (\psi ->
  PI_BzSyn $ BzS_Block (spos $ piTok $ head psi) (piSyns $ psi !! 1) )










parseBlock7 :: ParserOp
parseBlock7 = genericParseOp [mtk_StartDo, MP_Exs, MP_Bkx] (\psi ->
  PI_BzSyn $ BzS_Block (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 2]) )










parseBlock8 :: ParserOp
parseBlock8 = genericParseOp [mtk_StartDo, MP_Expr, MP_Bkx] (\psi ->
  PI_BzSyn $ BzS_Block (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 2]) )










parseBlock9 :: ParserOp
parseBlock9 = genericParseOp [mtk_StartDo, MP_Bkx] (\psi ->
  PI_BzSyn $ BzS_Block (spos $ piTok $ head psi) [piSyn $ psi !! 1] )










parseBlock :: Parser
parseBlock = Parser (\ps ->
  let parseFn = [parseBlock0 , parseBlock1 , parseBlock2 , parseBlock3 , parseBlock4 ,
                 parseBlock5 , parseBlock6 , parseBlock7 , parseBlock8 , parseBlock9 ]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left [] )










parseCallFuse0 :: ParserOp
parseCallFuse0 = genericParseOp [MP_SOF, MP_CallItem] (\psi ->
  PI_BzSyn $ BzS_Calls (pos $ piSyn $ psi !! 1) [piSyn $ psi !! 1] )










parseCallFuse1 :: ParserOp
parseCallFuse1 = genericParseOp [MP_Calls, MP_CallItem] (\psi ->
  PI_BzSyn $ BzS_Calls (pos $ piSyn $ psi !! 1) ((calls $ piSyn $ head psi) ++ [piSyn $ psi !! 1]) )










parseCallFuse2 :: ParserOp
parseCallFuse2 = genericParseOp [MP_Calls, MP_Exs] (\psi ->
  PI_BzSyn $ BzS_Calls (pos $ piSyn $ psi !! 1) ((calls $ piSyn $ head psi) ++ (piSyns $ psi !! 1)) )










parseCallFuse3 :: ParserOp
parseCallFuse3 = genericParseOp [MP_SOF, mtk_Newline] (\psi -> PI_SOF)










parseCallFuse :: Parser
parseCallFuse = Parser (\ps ->
  let parseFn = [parseCallFuse0, parseCallFuse1, parseCallFuse2, parseCallFuse3]
  in case tryParsers ps parseFn of
    Just pst -> Right pst
    Nothing  -> Left  [] )










parseCalls :: Parser
parseCalls = Parser (\ps ->
  case (runParsers ps [parseModifiers, parsePrimitives, parseSimplify, parseTupleEtc,
                       parseCompound, parsePolymorph, parseExpr, parseMisc,
                       parseTypeCall, parseLambda, parseFnCall, parseFnTy,
                       parseFnTyDef, parseBlock, parseCallFuse ]) of
    Left []   -> Left []
    Left err  -> Left err
    Right ps' -> Right ps' )
