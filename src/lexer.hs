module BzoLexer where
import BzoTypes
import Data.Char
import Data.Either
import Control.Monad
import Control.Applicative










data LexerState = LexerState{
  lsLine    :: Int,
  lsColumn  :: Int,
  lsOffset  :: Int,
  lsFName   :: String }
  deriving Eq










makeBzoPos :: LexerState -> BzoPos
makeBzoPos (LexerState l c o n) = BzoPos l c n










newtype Lexer a = Lexer { lex :: String -> LexerState -> Either BzoErr [(a, LexerState, String)] }










runLexer :: String -> Lexer a -> String -> Either BzoErr [a]
runLexer fname lx s =
  case (BzoLexer.lex lx s (LexerState 1 1 0 fname)) of
    Right [(ret, _, [] )] -> Right  [ret]
    Right [(_  , _, rem)] -> Left $ LexErr "Failed to consume entire input."
    Right lst             -> Right $ fst3 $ unzip3 lst
    Left  err             -> Left err
    where fst3 (a, b, c) = a










instance Functor Lexer where
  fmap f (Lexer lf) = Lexer (\s ls ->
    case lf s ls of
      Left  err -> Left  err
      Right lst -> Right [(f a, b, c) | (a, b, c) <- lst] )










instance Applicative Lexer where
  pure a = Lexer (\s ls -> Right [(a, ls, s)])
  (Lexer lf1) <*> (Lexer lf2) = Lexer (\s ls ->
    case (lf1 s ls) of
      Left  err            -> Left  err
      Right [(f, ls1, s1)] ->
        case (lf2 s1 ls1) of
          Left  err            -> Left  err
          Right [(a, ls2, s2)] -> Right [(f a, ls2, s2)] )










-- | Alternative to pure for applicative lexer
pureErr :: String -> Lexer a
pureErr err = Lexer (\s ls -> Left $ LexErr err)










concatMapWithErrs :: [a] -> (a -> Either b [c]) -> Either b [c]
concatMapWithErrs xs fn =
  let (ls, rs) = partitionEithers $ map fn xs
  in  if((length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ concat rs










instance Monad Lexer where
  return    = pure
  (>>=) p f = Lexer (\s ls ->
    let lf = (\(a, ls', s') -> BzoLexer.lex (f a) s' ls)
        ps = (BzoLexer.lex p) s ls
    in case ps of
      Left  err -> Left err
      Right xs  -> concatMapWithErrs xs lf )










instance MonadPlus Lexer where
  mzero = Lexer (\ls s -> Left (LexErr "Parse Failure"))  -- Add a makeErr function later
  mplus p q = Lexer (\s ls ->
    let ps = (BzoLexer.lex p s ls)
        qs = (BzoLexer.lex q s ls)
    in  case (ps, qs) of
      (Left err,        _) -> Left err
      (_       , Left err) -> Left err
      (Right  x, Right  y) -> Right (x ++ y))










instance Alternative Lexer where
  empty = mzero
  (<|>) p q = Lexer (\s ls ->
    case (BzoLexer.lex p s ls) of
      Left err -> (BzoLexer.lex q) s ls
      x        -> x )










moveOne :: LexerState -> Char -> LexerState
moveOne ls ch =
  let (LexerState l c o n) = ls
  in if(ch == '\n')
    then LexerState (l+1) c (o+1) n
    else LexerState l (c+1) (o+1) n










satisfy :: (Char -> Bool) -> Lexer Char
satisfy f = item >>= \ch ->
  if f ch
    then return ch
    else mzero










getLexerState :: Lexer LexerState
getLexerState = Lexer $ \s ls -> Right [(ls, ls, s)]










item :: Lexer Char
item = Lexer $ \s ls ->
  case s of
    []     -> Right []
    (c:cs) -> Right [(c, moveOne ls c, cs)]










lexChar :: Char -> Lexer Char
lexChar ch = satisfy (\c -> c == ch)










lexCharFrom :: [Char] -> Lexer Char
lexCharFrom cs = satisfy (\c -> elem c cs)










lexExceptChar :: Char -> Lexer Char
lexExceptChar ch = satisfy (\c -> c /= ch)










lexExceptCharFrom :: [Char] -> Lexer Char
lexExceptCharFrom cs = satisfy (\c -> not $ elem c cs)










toLstLexer :: Lexer a -> Lexer [a]
toLstLexer lx = Lexer (\s ls ->
  case (BzoLexer.lex lx s ls) of
    Left  err -> Left err
    Right lst -> Right $ map xform lst)
    where xform (a, b, c) = ([a], b, c)










alphaL     = "abcdefghijklmnopqrstuvwxyz"
alphaU     = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
numerals   = "0123456789"
symbols    = "`!#%^&*-=+|<>?/\\"
special    = "~@$(){}[]_:;',.\""
whitespace = " \t\v"
newline    = "\n\r"










lexGenString :: String -> Lexer String
lexGenString [] = return []
lexGenString (c:cs) = do { lexChar c; lexGenString cs; return (c:cs)}











lexComment :: Lexer String
lexComment = do
  st <- lexChar '"'
  cm <- many $ lexExceptChar '"'
  nd <- lexChar '"'
  return cm










lexGenEscape :: Char -> Char -> Lexer Char
lexGenEscape ch ret = do
  a <- lexChar '\\'
  b <- lexChar ch
  return ret










lexEscape :: Lexer Char
lexEscape =
  lexGenEscape 'n'  '\n' <|>
  lexGenEscape 'v'  '\v' <|>
  lexGenEscape 't'  '\t' <|>
  lexGenEscape 'r'  '\r' <|>
  lexGenEscape '\\' '\\' <|>
  lexGenEscape 'n'  '\n' <|>
  lexGenEscape '\'' '\'' <|>
  lexGenEscape '"'  '\"' <|>
  lexGenEscape 'a'  '\a'










lexString :: Lexer BzoToken
lexString = do
  pos <- getLexerState
  st  <- lexChar '\''
  sr  <- many (lexEscape <|> (lexExceptChar '\''))
  nd  <- lexChar '\''
  return $ TkStr (makeBzoPos pos) sr










lexWhiteSpace :: Lexer BzoToken
lexWhiteSpace = do
  a <- lexComment <|> (toLstLexer $ satisfy isSpace)
  return TkNil










lexStringToToken :: String -> (BzoPos -> BzoToken) -> Lexer BzoToken
lexStringToToken st f = do
  p <- getLexerState
  s <- lexGenString st
  return $ f (makeBzoPos p)










lexSymbol :: Lexer BzoToken
lexSymbol =
  (lexStringToToken "::" (\p -> TkDefine    p)) <|>
  (lexStringToToken ";;" (\p -> TkFnSym     p)) <|>
  (lexStringToToken "("  (\p -> TkStartTup  p)) <|>
  (lexStringToToken ")"  (\p -> TkEndTup    p)) <|>
  (lexStringToToken "["  (\p -> TkStartDat  p)) <|>
  (lexStringToToken "]"  (\p -> TkEndDat    p)) <|>
  (lexStringToToken "{"  (\p -> TkStartDo   p)) <|>
  (lexStringToToken "}"  (\p -> TkEndDo     p)) <|>
  (lexStringToToken ":"  (\p -> TkFilterSym p)) <|>
  (lexStringToToken ";"  (\p -> TkLambdaSym p)) <|>
  (lexStringToToken "."  (\p -> TkSepExpr   p)) <|>
  (lexStringToToken ","  (\p -> TkSepPoly   p)) <|>
  (lexStringToToken "()" (\p -> TkTupEmpt   p)) <|>
  (lexStringToToken "[]" (\p -> TkArrMod    p)) <|>
  (lexStringToToken "_"  (\p -> TkWildcard  p)) <|>
  (lexStringToToken "@"  (\p -> TkReference p)) <|>
  (lexStringToToken "~"  (\p -> TkMutable   p))










generalLexer :: Lexer BzoToken
generalLexer =
  lexWhiteSpace <|>
  lexSymbol     <|>
  lexString












lexFile :: String -> String -> Either BzoErr [BzoToken]
lexFile file syms = runLexer file generalLexer syms
