module Compiler where
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec hiding (try, spaces)
import Data.Functor.Identity
import Data.Either
import BzoParser
import BzoTypes
import BzoLexer
import BzoSyntax










--This function will eventually become the compiler pipeline
compileFile :: String -> Either [BzoErr] [BzoToken]
compileFile f = do
    let outs = fileLexer f
    let (ls, rs) = partitionEithers outs
    if length ls > 0
        then Left  $ map LexErr ls
        else Right $ concat rs










compileFile' :: String -> Either [BzoErr] [BzoProgram]
compileFile' f = do
    let lex = fileLexer f
    let (lls, lrs) = partitionEithers lex
    if length lls > 0
        then Left  $ map LexErr lls
        else Right $ map bzoParser lrs
            --let (pls, prs) = partitionEithers parse
            --if length pls > 0
            --    then Left  pls
            --    else Right prs
    
