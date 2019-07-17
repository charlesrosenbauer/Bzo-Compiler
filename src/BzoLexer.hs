module BzoLexer where
import BzoTypes
import Data.Char
import Data.List as L
import Data.Text as T
import Control.Monad
import Control.Applicative
import Tokens
import Error
import HigherOrder











data LexerState = LexerState{
  lsLine    :: Int,
  lsColumn  :: Int,
  lsOffset  :: Int,
  lsFName   :: Text }
  deriving Eq










makeBzoPos :: LexerState -> BzoPos
makeBzoPos (LexerState l c _ n) = BzoPos l c n










newtype Lexer a = Lexer { lex :: Text -> LexerState -> Either BzoErr [(a, LexerState, Text)] }










makeLexErr :: LexerState -> BzoErr
makeLexErr ls =
  let p = makeBzoPos ls
      l = line p
      c = column p
      n = fileName p
  in (LexErr (BzoPos l c n) $ pack "Lexer error\n")










runLexer :: Text -> Lexer a -> Text -> Either BzoErr a
runLexer fname lx s =
  let frames  = (BzoLexer.lex lx s (LexerState 1 1 0 fname))
      frames' = mapEither frames (L.map (\(a, b, c) -> (a, b, T.null c)))
  in case frames' of
      Right [(ret,  _, True)] -> Right  ret
      Right [(_  , st, _   )] -> Left $ LexErr (BzoPos (lsLine st) (lsColumn st) (lsFName st)) $ pack "Failed to consume entire input."
      Left  err               -> Left err
      _                       -> Left $ LexErr (BzoPos 0 0 fname) $ pack "Bug detected in runLexer in BzoLexer.hs"










instance Functor Lexer where
  fmap f (Lexer lf) = Lexer (\s ls ->
    case lf s ls of
      Left  err -> Left  err
      Right lst -> Right [(f a, b, c) | (a, b, c) <- lst] )










instance Applicative Lexer where
  pure a = Lexer (\s ls -> Right [(a, ls, s)])
  (Lexer lf1) <*> (Lexer lf2) = Lexer (\s ls ->
    case (lf1 s ls) of
      Left  err -> Left err
      Right lst ->
        let (p1, p23) = unzip $ L.map (\(a, b, c) -> (a, (c, b))) lst
            out1 = L.map (\(a, b) -> lf2 a b) p23
            out2 = sepErrs out1
        in case out2 of
          Left  err -> Left err
          Right val -> Right $ L.concat $ applyListFns p1 (\f (a, b, c) -> (f a, b, c)) val   )










instance Monad Lexer where
  return    = pure
  (>>=) p f = Lexer (\s ls ->
    let lf = (\(a, ls', s') -> BzoLexer.lex (f a) s' ls')
        ps = (BzoLexer.lex p) s ls
    in case ps of
      Left  err -> Left err
      Right xs  -> concatMapWithErrs xs lf )










instance MonadPlus Lexer where
  mzero = Lexer (\_ ls -> Left $ makeLexErr ls)
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
      Left  _   -> (BzoLexer.lex q) s ls
      Right []  -> (BzoLexer.lex q) s ls
      x         -> x )










moveOne :: LexerState -> Char -> LexerState
moveOne ls ch =
  let (LexerState l c o n) = ls
  in if(ch == '\n')
    then LexerState (l+1) 1 (o+1) n
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
  case (T.uncons s) of
    Nothing     -> Right []
    Just (c,cs) -> Right [(c, moveOne ls c, cs)]










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
    Right lst -> Right $ L.map xform lst)
    where xform (a, b, c) = ([a], b, c)










toTxtLexer :: Lexer Char -> Lexer Text
toTxtLexer lx = Lexer (\s ls ->
  case (BzoLexer.lex lx s ls) of
    Left  err -> Left err
    Right lst -> Right $ L.map xform lst)
    where xform (a, b, c) = (pack [a], b, c)










lexGenString :: Text -> Lexer Text
lexGenString t =
  case (T.uncons t) of
    Just (c,cs) -> do { _ <- lexChar c; _ <- lexGenString cs; return $ T.cons c cs}
    _           -> return t











lexComment :: Lexer Text
lexComment = do
  _  <- lexChar '"'
  cm <- many $ lexExceptChar '"'
  _  <- lexChar '"'
  return $ pack cm










lexGenEscape :: Char -> Char -> Lexer Char
lexGenEscape ch ret = do
  _ <- lexChar '\\'
  _ <- lexChar ch
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
  lexGenEscape '0'  '\o0'<|>
  lexGenEscape 'a'  '\a'










lexString :: Lexer BzoToken
lexString = do
  pos <- getLexerState
  _   <- lexChar '\''
  sr  <- many (lexEscape <|> (lexExceptChar '\''))
  _   <- lexChar '\''
  return $ TkStr (makeBzoPos pos) $ pack sr










lexWhiteSpace :: Lexer BzoToken
lexWhiteSpace = do
  _  <- lexComment <|> (toTxtLexer $ satisfy isSpace)
  return TkNil










lexStringToToken :: Text -> (BzoPos -> BzoToken) -> Lexer BzoToken
lexStringToToken st f = do
  p <- getLexerState
  _ <- lexGenString st
  return $ f (makeBzoPos p)










lexSymbol :: Lexer BzoToken
lexSymbol =
  (lexStringToToken (pack "..") (\p -> TkArrMod    p)) <|>
  (lexStringToToken (pack "::") (\p -> TkDefine    p)) <|>
  (lexStringToToken (pack ";;") (\p -> TkFnSym     p)) <|>
  (lexStringToToken (pack "()") (\p -> TkTupEmpt   p)) <|>
  (lexStringToToken (pack "[]") (\p -> TkArrGnrl   p)) <|>
  (lexStringToToken (pack "(")  (\p -> TkStartTup  p)) <|>
  (lexStringToToken (pack ")")  (\p -> TkEndTup    p)) <|>
  (lexStringToToken (pack "[")  (\p -> TkStartDat  p)) <|>
  (lexStringToToken (pack "]")  (\p -> TkEndDat    p)) <|>
  (lexStringToToken (pack "{")  (\p -> TkStartDo   p)) <|>
  (lexStringToToken (pack "}")  (\p -> TkEndDo     p)) <|>
  (lexStringToToken (pack ".")  (\p -> TkFilterSym p)) <|>
  (lexStringToToken (pack ";")  (\p -> TkLambdaSym p)) <|>
  (lexStringToToken (pack ":")  (\p -> TkSepExpr   p)) <|>
  (lexStringToToken (pack ",")  (\p -> TkSepPoly   p)) <|>
  (lexStringToToken (pack "_")  (\p -> TkWildcard  p)) <|>
  --(lexStringToToken (pack "`")  (\p -> TkCurrySym  p)) <|>
  (lexStringToToken (pack "@")  (\p -> TkReference p))










isLegalChar :: Char -> Bool
isLegalChar c = (isPrint c) && (not $ isSpace c) && (not $ elem c "~@_#()[]{}:;\'\",.")










lexIdentifier :: Lexer BzoToken
lexIdentifier = do
  p  <- getLexerState
  c0 <- satisfy (\c -> (isLegalChar c) && (not $ isUpper c) && (not $ isDigit c))
  cs <- many $ satisfy isLegalChar
  return (TkId (makeBzoPos p) $ pack (c0 : cs))










lexMutIdentifier :: Lexer BzoToken
lexMutIdentifier = do
  p  <- getLexerState
  c0 <- lexChar '~'
  c1 <- satisfy (\c -> (isLegalChar c) && (not $ isUpper c) && (not $ isDigit c))
  cs <- many $ satisfy isLegalChar
  return (TkMutId (makeBzoPos p) $ pack (c0 : (c1 : cs)))










lexTypeIdentifier :: Lexer BzoToken
lexTypeIdentifier = do
  p  <- getLexerState
  c0 <- satisfy isUpper
  cs <- many $ satisfy isLegalChar
  return (TkTypeId (makeBzoPos p) $ pack (c0 : cs))










lexTyVarIdentifier :: Lexer BzoToken
lexTyVarIdentifier = do
  p  <- getLexerState
  c0 <- satisfy isUpper
  cs <- many $ satisfy isLegalChar
  c' <- lexChar '\''
  return (TkTyVar (makeBzoPos p) $ pack (c0 : (cs ++ [c'])))









lexBIIdentifier :: Lexer BzoToken
lexBIIdentifier = do
  p  <- getLexerState
  b  <- lexChar '#'
  c0 <- satisfy (\c -> (isLegalChar c) && (not $ isUpper c) && (not $ isDigit c))
  cs <- many $ satisfy isLegalChar
  return (TkBuiltin (makeBzoPos p) $ pack (b : (c0 : cs)))










lexBITypeIdentifier :: Lexer BzoToken
lexBITypeIdentifier = do
  p  <- getLexerState
  b  <- lexChar '#'
  c0 <- satisfy isUpper
  cs <- many $ satisfy isLegalChar
  return (TkBIType (makeBzoPos p) $ pack (b : (c0 : cs)))










readIntWithBase :: String -> Integer -> Integer
readIntWithBase sr b =
  let rs = L.reverse sr
  in  (readDgt rs b)
    where readDgt [] _ = 0
          readDgt st base =
            let (s0 : ss) = st
                n = case s0 of
                  '0' -> 0
                  '1' -> 1
                  '2' -> 2
                  '3' -> 3
                  '4' -> 4
                  '5' -> 5
                  '6' -> 6
                  '7' -> 7
                  '8' -> 8
                  '9' -> 9
                  'a' -> 10
                  'A' -> 10
                  'b' -> 11
                  'B' -> 11
                  'c' -> 12
                  'C' -> 12
                  'd' -> 13
                  'D' -> 13
                  'e' -> 14
                  'E' -> 14
                  'f' -> 15
                  'F' -> 15
                  _   -> -1
            in (n + (base * (readDgt ss base)))










lexBinInt :: Lexer BzoToken
lexBinInt = do
  p  <- getLexerState
  _  <- ((lexGenString $ pack "0b") <|> (lexGenString $ pack "0B"))   --c0
  c1 <- some $ satisfy (\c -> elem c "01")
  return (TkInt (makeBzoPos p) (readIntWithBase c1 2))










lexOctInt :: Lexer BzoToken
lexOctInt = do
  p  <- getLexerState
  _  <- lexGenString $ pack "0"    -- c0
  c1 <- some $ satisfy isOctDigit
  return (TkInt (makeBzoPos p) (readIntWithBase c1 8))










lexDecInt :: Lexer BzoToken
lexDecInt = do
  p <- getLexerState
  c <- some $ satisfy isDigit
  return (TkInt (makeBzoPos p) (readIntWithBase c 10))










lexNegDecInt :: Lexer BzoToken
lexNegDecInt = do
  p <- getLexerState
  _ <- lexGenString $ pack "-"
  c <- some $ satisfy isDigit
  return (TkInt (makeBzoPos p) $ -1 * (readIntWithBase c 10))









lexHexInt :: Lexer BzoToken
lexHexInt = do
  p  <- getLexerState
  _  <- ((lexGenString $ pack "0x") <|> (lexGenString $ pack "0X"))   -- c0
  c1 <- some $ satisfy isHexDigit
  return (TkInt (makeBzoPos p) (readIntWithBase c1 16))










lexFlt :: Lexer BzoToken
lexFlt = do
  p  <- getLexerState
  c0 <- some $ satisfy isDigit
  _  <- lexGenString $ pack "."      -- c1
  c2 <- some $ satisfy isDigit
  return (TkFlt (makeBzoPos p) ((fromIntegral $ readIntWithBase c0 10) +
    ((fromIntegral $ readIntWithBase c2 10) / (10 ^ (L.length c2))) ) )










lexNegFlt :: Lexer BzoToken
lexNegFlt = do
  p  <- getLexerState
  _  <- lexGenString $ pack "-"
  c0 <- some $ satisfy isDigit
  _  <- lexGenString $ pack "."      -- c1
  c2 <- some $ satisfy isDigit
  return (TkFlt (makeBzoPos p) $ -1 * ((fromIntegral $ readIntWithBase c0 10) +
    ((fromIntegral $ readIntWithBase c2 10) / (10 ^ (L.length c2))) ) )










lexNewline :: Lexer BzoToken
lexNewline = do
  p <- getLexerState
  _ <- some $ lexChar '\n'
  return (TkNewline (makeBzoPos p))









generalLexer :: Lexer BzoToken
generalLexer =
  lexNewline          <|>
  lexWhiteSpace       <|>
  lexMutIdentifier    <|>
  lexNegFlt           <|>
  lexNegDecInt        <|>
  lexSymbol           <|>
  lexString           <|>
  lexBITypeIdentifier <|>
  lexBIIdentifier     <|>
  lexTyVarIdentifier  <|>
  lexTypeIdentifier   <|>
  lexIdentifier       <|>
  lexFlt              <|>
  lexBinInt           <|>
  lexOctInt           <|>
  lexHexInt           <|>
  lexDecInt










generalLexerMany :: Lexer [BzoToken]
generalLexerMany = many generalLexer










literateTransform :: Text -> Text
literateTransform text =
  let l  = T.lines text
      ls = L.map (\x -> if((T.length x > 0) && (T.head x == '>'))
                          then ((pack " ") `T.append` (T.drop 1 x) `T.append` (pack "\n"))
                          else (pack "\n")) l
  in T.concat ls










tryLiterateTransform :: FilePath -> Text -> Text
tryLiterateTransform file text =
  if T.isSuffixOf (pack ".lbz") $ pack file
    then literateTransform text
    else text










removeLexerArtifacts :: BzoToken -> BzoToken -> BzoToken
removeLexerArtifacts prev this =
  case (prev, this) of
    ((TkNewline _), (TkNewline _)) -> TkNil
    (TkNil        , tk1          ) -> tk1
    (_ {- tk0 -}  , tk1          ) -> tk1










fileLexer :: Text -> FilePath -> Either [BzoErr] [BzoToken]
fileLexer syms file =
  let text = tryLiterateTransform file syms
      res  = runLexer (pack file) generalLexerMany text
  in  case res of
    Left  err -> Left [err]
    Right tks -> Right $ repeatUntilFilterStops isValidToken (L.scanl removeLexerArtifacts TkNil) $ L.filter isValidToken tks
  where isValidToken (TkNil) = False
        isValidToken _       = True
