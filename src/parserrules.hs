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










parseCompound6 :: ParserOp
parseCompound6 = genericParseOp [mtk_StartTup, MP_Cpxs, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ((piSyns $ psi !! 1) ++ [piSyn $ psi !! 2]) )










parseCompound7 :: ParserOp
parseCompound7 = genericParseOp [mtk_StartTup, MP_Cpx, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Cmpd (spos $ piTok $ head psi) ([piSyn $ psi !! 1] ++ [piSyn $ psi !! 2]) )










parseCompoundErr0 :: ParserOp
parseCompoundErr0 = genericParseOp [MP_Cpx, MP_Plx] (\psi -> PI_Err "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr1 :: ParserOp
parseCompoundErr1 = genericParseOp [MP_Cpxs, MP_Plx] (\psi -> PI_Err "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr2 :: ParserOp
parseCompoundErr2 = genericParseOp [MP_Cpx, MP_Plxs] (\psi -> PI_Err "Invalid combination of polymorphic and compound expressions.")










parseCompoundErr3 :: ParserOp
parseCompoundErr3 = genericParseOp [MP_Cpxs, MP_Plxs] (\psi -> PI_Err "Invalid combination of polymorphic and compound expressions.")










parseCompound :: Parser
parseCompound = Parser (\ps ->
  let parseFn = [parseCompound0, parseCompound1, parseCompound2,
                 parseCompound3, parseCompound4, parseCompound5,
                 parseCompound6, parseCompound7]
      errFn = [parseCompoundErr0, parseCompoundErr1, parseCompoundErr2, parseCompoundErr3]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just ps,      _ ) -> Right ps
    (Nothing, Nothing) -> Left []
    (Nothing, Just er) -> Left [ParseErr (piErr $ head $ stack er)])










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










parsePolymorphErr0 :: ParserOp
parsePolymorphErr0 = genericParseOp [MP_Plx, MP_Cpx] (\psi -> PI_Err "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr1 :: ParserOp
parsePolymorphErr1 = genericParseOp [MP_Plxs, MP_Cpx] (\psi -> PI_Err "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr2 :: ParserOp
parsePolymorphErr2 = genericParseOp [MP_Plx, MP_Cpxs] (\psi -> PI_Err "Invalid combination of polymorphic and compound expressions.")










parsePolymorphErr3 :: ParserOp
parsePolymorphErr3 = genericParseOp [MP_Plxs, MP_Cpxs] (\psi -> PI_Err "Invalid combination of polymorphic and compound expressions.")










parsePolymorph :: Parser
parsePolymorph = Parser (\ps ->
  let parseFn = [parsePolymorph0, parsePolymorph1, parsePolymorph2,
                 parsePolymorph3, parsePolymorph4, parsePolymorph5,
                 parsePolymorph6, parsePolymorph7]
      errFn   = [parsePolymorphErr0, parsePolymorphErr1, parsePolymorphErr2, parsePolymorphErr3]
  in case (tryParsers ps parseFn, tryParsers ps errFn) of
    (Just ps,      _ ) -> Right ps
    (Nothing, Nothing) -> Left []
    (Nothing, Just er) -> Left [ParseErr (piErr $ head $ stack er)])










parseTupleEtc0 :: ParserOp
parseTupleEtc0 = genericParseOp [MP_Item, mtk_EndTup] (\psi ->
  PI_TX $ BzS_Expr (pos $ piSyn $ head psi) [piSyn $ head psi] )










parseTupleEtc1 :: ParserOp
parseTupleEtc1 = genericParseOp [MP_Item, MP_Tx] (\psi ->
  PI_TX $ BzS_Expr (pos $ piSyn $ head psi) ([piSyn $ head psi] ++ (exprs $ piSyn $ psi !! 1)) )










parseTupleEtc2 :: ParserOp
parseTupleEtc2 = genericParseOp [mtk_StartTup, MP_Tx] (\psi ->
  PI_BzSyn $ BzS_Box (spos $ piTok $ head psi) (piSyn $ psi !! 1) )










parseTupleEtc :: Parser
parseTupleEtc = Parser (\ps ->
  let parseFn = [parseTupleEtc0, parseTupleEtc1, parseTupleEtc2]
  in case tryParsers ps parseFn of
    Just ps -> Right ps
    Nothing -> Left  [] )









parseCalls :: Parser
parseCalls = Parser (\ps ->
  case (runParsers ps [parsePrimitives, parseCompound, parsePolymorph, parseTupleEtc]) of
    Left []   -> Left []
    Left err  -> Left err
    Right ps' -> Right ps' )
