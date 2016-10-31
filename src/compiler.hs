module Compiler where
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec hiding (try, spaces)
import Data.Functor.Identity
import Data.Either
import BzoTypes
import BzoParser










--This function will eventually become the compiler pipeline
compileFile :: String -> Either [BzoErr] [Token]
compileFile f = do
    let outs = fileLexer f
    let (ls, rs) = partitionEithers outs
    if length ls > 0
        then Left  $ map LexErr ls
        else Right $ concat rs
    
