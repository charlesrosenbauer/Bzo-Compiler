module BzoLexer where
import BzoTypes
import Data.Char
import Data.Either
import Data.List
import Control.Monad
import Control.Applicative
import Debug.Trace










data LexerState = LexerState{
  lsLine    :: Int,
  lsColumn  :: Int,
  lsOffset  :: Int,
  lsFName   :: String }
  deriving Eq










makeBzoPos :: LexerState -> BzoPos
makeBzoPos (LexerState l c o n) = BzoPos l c n










newtype Lexer a = Lexer { lex :: String -> LexerState -> Either BzoErr [(a, LexerState, String)] }










makeLexErr :: LexerState -> BzoErr
makeLexErr ls =
  let p = makeBzoPos ls
      l = line p
      c = column p
      n = fileName p
  in (LexErr (BzoPos l c n) "Lexer error\n")










runLexer :: String -> Lexer a -> String -> Either BzoErr a
runLexer fname lx s =
  case (BzoLexer.lex lx s (LexerState 1 1 0 fname)) of
    Right [(ret, _, [] )] -> Right  ret
    Right [(_  , s, rem)] -> Left $ LexErr (BzoPos (lsLine s) (lsColumn s) (lsFName s)) "Failed to consume entire input."
    Left  err             -> Left err










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
        let (p1, p23) = unzip $ map (\(a, b, c) -> (a, (c, b))) lst
            out1 = map (\(a, b) -> lf2 a b) p23
            out2 = sepErrs out1
        in case out2 of
          Left  err -> Left err
          Right val -> Right $ concat $ applyListFns p1 (\f (a, b, c) -> (f a, b, c)) val   )










-- | Alternative to pure for applicative lexer. Not sure if it's necessary.
pureErr :: String -> Lexer a
pureErr err = Lexer (\s ls -> Left $ LexErr (BzoPos (lsLine ls) (lsColumn ls) (lsFName ls)) err)










mapEither :: Either a b -> (b -> c) -> Either a c
mapEither e f =
  case e of
    Left  err -> Left err
    Right val -> Right $ f val










applyListFns :: [(b -> c)] -> ((b -> c) -> a -> d) -> [[a]] -> [[d]]
applyListFns fs fn xs = map (\(f, x) -> map (fn f) x) $ zip fs xs










sepErrs :: [Either a [b]] -> Either a [[b]]
sepErrs xs =
  let (ls, rs) = partitionEithers xs
  in  if((length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ rs










concatMapWithErrs' :: Either a [b] -> (b -> Either a [c]) -> Either a [c]
concatMapWithErrs' xs fn =
  case xs of
    Left  err -> Left err
    Right val ->
      let (ls, rs) = partitionEithers $ map fn val
      in  if((length ls) /= 0)
        then Left  $ ls !! 0
        else Right $ concat rs










concatMapWithErrs :: [a] -> (a -> Either b [c]) -> Either b [c]
concatMapWithErrs xs fn =
  let (ls, rs) = partitionEithers $ map fn xs
  in  if((length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ concat rs










instance Monad Lexer where
  return    = pure
  (>>=) p f = Lexer (\s ls ->
    let lf = (\(a, ls', s') -> BzoLexer.lex (f a) s' ls')
        ps = (BzoLexer.lex p) s ls
    in case ps of
      Left  err -> Left err
      Right xs  -> concatMapWithErrs xs lf )










instance MonadPlus Lexer where
  mzero = Lexer (\s ls -> Left $ makeLexErr ls)
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
      Left  err -> (BzoLexer.lex q) s ls
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










lexGenString :: String -> Lexer String
lexGenString [] = return []
lexGenString (c:cs) = do { lexChar c; lexGenString cs; return (c:cs)}











lexComment :: Lexer String
lexComment = do
  st <- lexChar '"'
  cm <- many $ lexExceptChar '"'
  nd <- lexChar '"'
  --nl <- many $ lexChar '\n'
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
  lexGenEscape '0'  '\o0'<|>
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
  (lexStringToToken "()" (\p -> TkTupEmpt   p)) <|>
  (lexStringToToken "[]" (\p -> TkArrGnrl   p)) <|>
  (lexStringToToken "("  (\p -> TkStartTup  p)) <|>
  (lexStringToToken ")"  (\p -> TkEndTup    p)) <|>
  (lexStringToToken "["  (\p -> TkStartDat  p)) <|>
  (lexStringToToken "]"  (\p -> TkEndDat    p)) <|>
  (lexStringToToken "{"  (\p -> TkStartDo   p)) <|>
  (lexStringToToken "}"  (\p -> TkEndDo     p)) <|>
  (lexStringToToken ":"  (\p -> TkFilterSym p)) <|>
  (lexStringToToken ";"  (\p -> TkLambdaSym p)) <|>
  (lexStringToToken ".." (\p -> TkArrMod    p)) <|>
  (lexStringToToken "."  (\p -> TkSepExpr   p)) <|>
  (lexStringToToken ","  (\p -> TkSepPoly   p)) <|>
  (lexStringToToken "_"  (\p -> TkWildcard  p)) <|>
  (lexStringToToken "@"  (\p -> TkReference p))










isLegalChar :: Char -> Bool
isLegalChar c = (isPrint c) && (not $ isSpace c) && (not $ elem c "~@_$()[]{}:;\'\",.")










lexIdentifier :: Lexer BzoToken
lexIdentifier = do
  p  <- getLexerState
  c0 <- satisfy (\c -> (isLegalChar c) && (not $ isUpper c) && (not $ isDigit c))
  cs <- many $ satisfy isLegalChar
  return (TkId (makeBzoPos p) (c0 : cs))










lexMutIdentifier :: Lexer BzoToken
lexMutIdentifier = do
  p  <- getLexerState
  c0 <- lexChar '~'
  c1 <- satisfy (\c -> (isLegalChar c) && (not $ isUpper c) && (not $ isDigit c))
  cs <- many $ satisfy isLegalChar
  return (TkMutId (makeBzoPos p) (c0 : (c1 : cs)))










lexTypeIdentifier :: Lexer BzoToken
lexTypeIdentifier = do
  p  <- getLexerState
  c0 <- satisfy isUpper
  cs <- many $ satisfy isLegalChar
  return (TkTypeId (makeBzoPos p) (c0 : cs))










lexBIIdentifier :: Lexer BzoToken
lexBIIdentifier = do
  p  <- getLexerState
  b  <- lexChar '$'
  c0 <- satisfy (\c -> (isLegalChar c) && (not $ isUpper c) && (not $ isDigit c))
  cs <- many $ satisfy isLegalChar
  return (TkBuiltin (makeBzoPos p) (b : (c0 : cs)))










lexBITypeIdentifier :: Lexer BzoToken
lexBITypeIdentifier = do
  p  <- getLexerState
  b  <- lexChar '$'
  c0 <- satisfy isUpper
  cs <- many $ satisfy isLegalChar
  return (TkBIType (makeBzoPos p) (b : (c0 : cs)))










readIntWithBase :: String -> Integer -> Integer
readIntWithBase sr b =
  let rs = reverse sr
  in  (readDgt rs b)
    where readDgt [] b = 0
          readDgt st b =
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
            in (n + (b * (readDgt ss b)))










lexBinInt :: Lexer BzoToken
lexBinInt = do
  p  <- getLexerState
  c0 <- ((lexGenString "0b") <|> (lexGenString "0B"))
  c1 <- some $ satisfy (\c -> elem c "01")
  return (TkInt (makeBzoPos p) (readIntWithBase c1 2))










lexOctInt :: Lexer BzoToken
lexOctInt = do
  p  <- getLexerState
  c0 <- lexGenString "0"
  c1 <- some $ satisfy isOctDigit
  return (TkInt (makeBzoPos p) (readIntWithBase c1 8))










lexDecInt :: Lexer BzoToken
lexDecInt = do
  p <- getLexerState
  c <- some $ satisfy isDigit
  return (TkInt (makeBzoPos p) (readIntWithBase c 10))










lexHexInt :: Lexer BzoToken
lexHexInt = do
  p  <- getLexerState
  c0 <- ((lexGenString "0x") <|> (lexGenString "0X"))
  c1 <- some $ satisfy isHexDigit
  return (TkInt (makeBzoPos p) (readIntWithBase c1 16))










lexFlt :: Lexer BzoToken
lexFlt = do
  p  <- getLexerState
  c0 <- some $ satisfy isDigit
  c1 <- lexGenString "."
  c2 <- some $ satisfy isDigit
  return (TkFlt (makeBzoPos p) ((fromIntegral $ readIntWithBase c0 10) +
    ((fromIntegral $ readIntWithBase c2 10) / (10 ^ (length c2))) ) )










lexNewline :: Lexer BzoToken
lexNewline = do
  p <- getLexerState
  c <- some $ lexChar '\n'
  return (TkNewline (makeBzoPos p))









generalLexer :: Lexer BzoToken
generalLexer =
  lexNewline          <|>
  lexWhiteSpace       <|>
  lexMutIdentifier    <|>
  lexSymbol           <|>
  lexString           <|>
  lexBITypeIdentifier <|>
  lexBIIdentifier     <|>
  lexTypeIdentifier   <|>
  lexIdentifier       <|>
  lexFlt              <|>
  lexBinInt           <|>
  lexOctInt           <|>
  lexHexInt           <|>
  lexDecInt










generalLexerMany :: Lexer [BzoToken]
generalLexerMany = many generalLexer










literateTransform :: String -> String
literateTransform text =
  let l  = lines text
      ls = map (\x -> if((length x > 0) && (head x == '>'))
                        then (" " ++ (drop 1 x) ++ "\n")
                        else ("\n")) l
  in concat ls










tryLiterateTransform :: FilePath -> String -> String
tryLiterateTransform file text =
  if isSuffixOf ".lbz" file
    then literateTransform text
    else text










removeLexerArtifacts :: BzoToken -> BzoToken -> BzoToken
removeLexerArtifacts prev this =
  case (prev, this) of
    ((TkNewline _), (TkNewline _)) -> TkNil
    (TkNil        , tk1          ) -> tk1
    (tk0          , tk1          ) -> tk1










fileLexer :: String -> FilePath -> Either [BzoErr] [BzoToken]
fileLexer syms file =
  let text = tryLiterateTransform file syms
      res  = runLexer file generalLexerMany text
  in  case res of
    Left  err -> Left [err]
    Right tks -> Right $ filter isValidToken $ scanl removeLexerArtifacts TkNil $ filter isValidToken tks
  where isValidToken (TkNil) = False
        isValidToken _       = True
