module BzoLexer where
import BzoTypes










data LexerState = LexerState{
  lsLine    :: Int,
  lsColumn  :: Int,
  lsOffset  :: Int,
  lsFName   :: FileName,
  lsFData   :: String }










lexChar :: LexerState -> Char -> Either Nothing LexerState
lexChar ls c =
  let (x : xs) = lsFData ls
  let (l c o n _) = ls
  let (l' c') = if(c == '\n')
    then (l+1, 0)
    else (l, c+1)
  if(c == x)
    then Left
    else Right $ LexerState l' c' (o+1) n xs












lexFile :: FileName -> String -> [BzoToken]
lexFile file syms =
