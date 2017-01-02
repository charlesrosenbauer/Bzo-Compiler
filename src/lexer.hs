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










newtype Lexer a = Lexer { lex :: String -> LexerState -> Either BzoErr [(a, LexerState, String)] }










runLexer :: String -> Lexer a -> String -> Either BzoErr a
runLexer fname lx s =
  case (BzoLexer.lex lx s (LexerState 1 1 0 fname)) of
    Right [(ret, _, [] )] -> Right  ret
    Right [(_  , _, rem)] -> Left $ LexErr "Failed to consume entire input."
    Left  err             -> Left err










moveOne :: LexerState -> Char -> LexerState
moveOne ls ch =
  let (LexerState l c o n) = ls
  in if(ch == '\n')
    then LexerState (l+1) c (o+1) n
    else LexerState l (c+1) (o+1) n










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










satisfy :: (Char -> Bool) -> Lexer Char
satisfy f = item >>= \ch ->
  if f ch
    then return ch
    else mzero










item :: Lexer Char
item = Lexer $ \s ls ->
  case s of
    []     -> Right []
    (c:cs) -> Right [(c, moveOne ls c, cs)]










lexChar :: Char -> Lexer Char
lexChar ch = satisfy (\c -> c == ch)










lexCharFrom :: [Char] -> Lexer Char
lexCharFrom cs = satisfy (\c -> elem c cs)










alphaL     = "abcdefghijklmnopqrstuvwxyz"
alphaU     = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
numerals   = "0123456789"
printable  = "`!#%^&*-=+|<>?/\\"
whitespace = " \n\t\v"











--lexFile :: FileName -> String -> [BzoToken]
--lexFile file syms =
