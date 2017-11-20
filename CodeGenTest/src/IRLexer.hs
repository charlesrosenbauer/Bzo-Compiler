module IRLexer where
import Data.Char
import Data.List as L
import Data.Either as E
import Control.Monad
import Control.Applicative
import Data.Text










sepErrs :: [Either a [b]] -> Either a [[b]]
sepErrs xs =
  let (ls, rs) = E.partitionEithers xs
  in  if((L.length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ rs










applyListFns :: [(b -> c)] -> ((b -> c) -> a -> d) -> [[a]] -> [[d]]
applyListFns fs fn xs = L.map (\(f, x) -> L.map (fn f) x) $ L.zip fs xs










concatMapWithErrs :: [a] -> (a -> E.Either b [c]) -> E.Either b [c]
concatMapWithErrs xs fn =
  let (ls, rs) = E.partitionEithers $ L.map fn xs
  in  if((L.length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ L.concat rs










data IRPos = IRPos{
  irLine   :: Int,
  irColumn :: Int,
  irFName  :: Text }
  deriving (Eq, Show)










data IRErr = IRErr IRPos Text










data IRToken
  = FuncToken   IRPos Text
  | NodeToken   IRPos Int
  | TypeToken   IRPos Text
  | ExternToken IRPos Text
  | ProcToken   IRPos Text
  | OpenBrace   IRPos
  | CloseBrace  IRPos
  | DefFunc     IRPos
  | DefType     IRPos
  | DefExtern   IRPos
  | DefProc     IRPos
  | NewLine     IRPos
  | PtrToken    IRPos
  | NilToken
  deriving (Eq, Show)










data IRLexerState = IRLexerState{
  lxLine    :: Int,
  lxColumn  :: Int,
  lxOffset  :: Int,
  lxFName   :: Text }
  deriving Eq










makeIRPos :: IRLexerState -> IRPos
makeIRPos (IRLexerState l c _ n) = IRPos l c n










newtype IRLexer a = IRLexer { lex :: String -> IRLexerState -> Either IRErr [(a, IRLexerState, String)] }










makeLexErr :: IRLexerState -> IRErr
makeLexErr ls =
  let p = makeIRPos ls
      l = irLine p
      c = irColumn p
      n = irFName p
  in (IRErr (IRPos l c n) $ pack "Lexer error\n")










runLexer :: String -> IRLexer a -> String -> Either IRErr a
runLexer fname lx s =
  case (IRLexer.lex lx s (IRLexerState 1 1 0 $ pack fname)) of
    Right [(ret,  _, [] )] -> Right  ret
    Right [(_  , st, _  )] -> Left $ IRErr (IRPos (lxLine st) (lxColumn st) (lxFName st)) $ pack "Failed to consume entire input."
    Left  err              -> Left err
    _                      -> Left $ IRErr (IRPos 0 0 $ pack fname) $ pack "Bug detected in runLexer in BzoLexer.hs"










instance Functor IRLexer where
  fmap f (IRLexer lf) = IRLexer (\s ls ->
    case lf s ls of
      Left  err -> Left  err
      Right lst -> Right [(f a, b, c) | (a, b, c) <- lst] )










instance Applicative IRLexer where
  pure a = IRLexer (\s ls -> Right [(a, ls, s)])
  (IRLexer lf1) <*> (IRLexer lf2) = IRLexer (\s ls ->
    case (lf1 s ls) of
      Left  err -> Left err
      Right lst ->
        let (p1, p23) = unzip $ L.map (\(a, b, c) -> (a, (c, b))) lst
            out1 = L.map (\(a, b) -> lf2 a b) p23
            out2 = sepErrs out1
        in case out2 of
          Left  err -> Left err
          Right val -> Right $ L.concat $ applyListFns p1 (\f (a, b, c) -> (f a, b, c)) val   )










instance Monad IRLexer where
  return    = pure
  (>>=) p f = IRLexer (\s ls ->
    let lf = (\(a, ls', s') -> IRLexer.lex (f a) s' ls')
        ps = (IRLexer.lex p) s ls
    in case ps of
      Left  err -> Left err
      Right xs  -> concatMapWithErrs xs lf )










instance MonadPlus IRLexer where
  mzero = IRLexer (\_ ls -> Left $ makeLexErr ls)
  mplus p q = IRLexer (\s ls ->
    let ps = (IRLexer.lex p s ls)
        qs = (IRLexer.lex q s ls)
    in  case (ps, qs) of
      (Left err,        _) -> Left err
      (_       , Left err) -> Left err
      (Right  x, Right  y) -> Right (x ++ y))










instance Alternative IRLexer where
  empty = mzero
  (<|>) p q = IRLexer (\s ls ->
    case (IRLexer.lex p s ls) of
      Left  _   -> (IRLexer.lex q) s ls
      Right []  -> (IRLexer.lex q) s ls
      x         -> x )










moveOne :: IRLexerState -> Char -> IRLexerState
moveOne (IRLexerState l c o n) ch =
  if(ch == '\n')
    then IRLexerState (l+1) 1 (o+1) n
    else IRLexerState l (c+1) (o+1) n










satisfy :: (Char -> Bool) -> IRLexer Char
satisfy f = item >>= \ch ->
  if f ch
    then return ch
    else mzero










getLexerState :: IRLexer IRLexerState
getLexerState = IRLexer $ \s ls -> Right [(ls, ls, s)]










item :: IRLexer Char
item = IRLexer $ \s ls ->
  case s of
    []     -> Right []
    (c:cs) -> Right [(c, moveOne ls c, cs)]










lexChar :: Char -> IRLexer Char
lexChar ch = satisfy (\c -> c == ch)










lexCharFrom :: [Char] -> IRLexer Char
lexCharFrom cs = satisfy (\c -> elem c cs)










lexExceptChar :: Char -> IRLexer Char
lexExceptChar ch = satisfy (\c -> c /= ch)










lexExceptCharFrom :: [Char] -> IRLexer Char
lexExceptCharFrom cs = satisfy (\c -> not $ elem c cs)










toLstLexer :: IRLexer a -> IRLexer [a]
toLstLexer lx = IRLexer (\s ls ->
  case (IRLexer.lex lx s ls) of
    Left  err -> Left err
    Right lst -> Right $ L.map xform lst)
    where xform (a, b, c) = ([a], b, c)










lexGenString :: String -> IRLexer String
lexGenString [] = return []
lexGenString (c:cs) = do { _ <- lexChar c; _ <- lexGenString cs; return (c:cs)}










lexStringToToken :: String -> (IRPos -> IRToken) -> IRLexer IRToken
lexStringToToken st f = do
  p <- getLexerState
  _ <- lexGenString st
  return $ f (makeIRPos p)










lexSymbol :: IRLexer IRToken
lexSymbol =
  (lexStringToToken "@"  (\p -> DefFunc     p)) <|>
  (lexStringToToken "%"  (\p -> DefType     p)) <|>
  (lexStringToToken "^"  (\p -> DefExtern   p)) <|>
  (lexStringToToken "{"  (\p -> OpenBrace   p)) <|>
  (lexStringToToken "}"  (\p -> CloseBrace  p)) <|>
  (lexStringToToken "~"  (\p -> DefProc     p)) <|>
  (lexStringToToken "*"  (\p -> PtrToken    p)) <|>
  (lexStringToToken "\n" (\p -> NewLine     p))










lexFunc :: IRLexer IRToken
lexFunc = do
  p  <- getLexerState
  c0 <- satisfy isLower
  cs <- many $ satisfy isAlphaNum
  return (FuncToken (makeIRPos p) $ pack (c0 : cs))










lexType :: IRLexer IRToken
lexType = do
  p  <- getLexerState
  c0 <- satisfy isUpper
  cs <- many $ satisfy isAlphaNum
  return (TypeToken (makeIRPos p) $ pack (c0 : cs))










lexNode :: IRLexer IRToken
lexNode = do
  p  <- getLexerState
  c0 <- lexChar '#'
  cs <- many $ satisfy isDigit
  return (NodeToken (makeIRPos p) $ readInt cs)










lexExtn :: IRLexer IRToken
lexExtn = do
  p  <- getLexerState
  c0 <- lexChar ':'
  c1 <- satisfy isLower
  cs <- many $ satisfy isAlphaNum
  return (FuncToken (makeIRPos p) $ pack (c0 : c1 : cs))










lexProc :: IRLexer IRToken
lexProc = do
  p  <- getLexerState
  c0 <- lexChar '!'
  c1 <- satisfy isLower
  cs <- many $ satisfy isAlphaNum
  return (FuncToken (makeIRPos p) $ pack (c0 : c1 : cs))










readInt :: String -> Int
readInt sr =
  let rs = L.reverse sr
  in  (readDgt rs)
    where readDgt [] = 0
          readDgt st =
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
              in (n + (10 * (readDgt ss)))










lexWhiteSpace :: IRLexer IRToken
lexWhiteSpace = do
  _  <- (toLstLexer $ satisfy isSpace)
  return NilToken










lexToken :: IRLexer IRToken
lexToken =
  lexNode    <|>
  lexFunc    <|>
  lexType    <|>
  lexExtn    <|>
  lexProc    <|>
  lexSymbol  <|>
  lexWhiteSpace










lexFile :: String -> String -> Either IRErr [IRToken]
lexFile fcontents fname =
  let tokens  = runLexer fname (many lexToken) fcontents
      tokens' = L.filter notNil $ L.head $ E.rights [tokens]
  in case tokens of
      Left errs -> Left  errs
      Right tks -> Right tokens'

    where notNil NilToken = False
          notNil _        = True