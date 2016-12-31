module BzoLexer where
import BzoTypes










data LexerState = LexerState{
  lsLine    :: Int,
  lsColumn  :: Int,
  lsOffset  :: Int,
  lsFName   :: FileName,
  lsFData   :: String }










-- | In this case, if line, column, offset, and file are equal, the data is as
-- | well. Just save some time by not checking the entire list.
instance Eq LexerState where
  (LexerState la, ca, oa, fa, _) == (LexerState lb, cb, ob, fb, _) =
    (la == lb) && (ca == cb) && (oa == ob) && (fa == fb)










lexChar :: LexerState -> Char -> (Bool, LexerState)
lexChar ls ch =
  let (x : xs) = lsFData ls
  let (l c o n _) = ls
  let (l' c') = if(c == '\n')
    then (l+1, 0)
    else (l, c+1)
  if(ch == x)
    then (True , ls)
    else (False, LexerState l' c' (o+1) n xs)










lexCharFrom :: LexerState -> [Char] -> (Bool, LexerState)
lexCharFrom ls cs =
  let (x : xs) = lsFData ls
  let (l c o n _) = ls
  let (l' c') = if(c == '\n')
    then (l+1, 0)
    else (l, c+1)
  if(elem x cs)
    then (True,  ls)
    else (False, LexerState l' c' (o+1) n xs)










applyMany :: a -> b -> (a -> b -> (Bool, a)) -> a
applyMany x y f =
  let (bl, x') = f x y
  if(bl)
    then applyMany x' y f
    else x'










lexManyChar :: LexerState -> Char -> (Bool, LexerState)
lexManyChar ls c =
  let a = applyMany ls c lexChar
  if(a == ls)
    then (False, a)
    else (True , a)










lexManyCharFrom :: LexerState -> [Char] -> (Bool, LexerState)
lexManyCharFrom ls cs =
  let a = applyMany ls cs lexCharFrom
  if(a == ls)
    then (False, a)
    else (True , a)










lexFile :: FileName -> String -> [BzoToken]
lexFile file syms =
