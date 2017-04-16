module BzoConfigParser where
import BzoParser
import BzoTypes
import BzoSyntax










parseLibCfg0 :: ParserOp
parseLibCfg0 = genericParseOp [mtk_TypeId, mtk_Str, mtk_Newline] (\psi ->
  PI_Cfg (LibLine (spos $ piTok $ head psi) (valId $ piTok $ head psi) (valStr $ piTok $ psi !! 1)))










parseLibCfg1 :: ParserOp
parseLibCfg1 = genericParseOp [MP_Cfg_Line, MP_Cfg_Line] (\psi ->
  PI_Cfg (LibLines (cpos $ piCfg $ head psi) ([piCfg $ head psi] ++ [piCfg $ psi !! 1])))










parseLibCfg2 :: ParserOp
parseLibCfg2 = genericParseOp [MP_Cfg_Lines, MP_Cfg_Line] (\psi ->
  PI_Cfg (LibLines (cpos $ piCfg $ head psi) ([piCfg $ head psi] ++ (libLines $ piCfg $ psi !! 1))))










parseLibCfg :: Parser
parseLibCfg = Parser (\ps ->
  let parseFn = [parseLibCfg0, parseLibCfg1, parseLibCfg2]
  in case tryParsers ps parseFn of
      Just pst -> Right pst
      Nothing  -> Left [] )










parseLibCfgFile :: String -> [BzoToken] -> Either [BzoErr] BzoSyntax
parseLibCfgFile f tks =
  case (parseIter (ParserState f (BzoPos 0 0 f) [] tks) [parseLibCfg]) of
      Left errs -> Left errs
      Right ast -> Right piCfg $ ast
