module Compiler where
import System.IO
import System.Directory
import Control.Monad
import Data.Tuple
import BzoParser
import BzoTypes
import BzoLexer
import BzoSyntax
import BzoTokens
import BzoPreprocessor
import BzoParameterParser
import BzoChecker
import BzoTypeModeller
import HigherOrder










compileFilePass :: BzoSettings -> IO ()
compileFilePass (BzoSettings imp lib flg opt pfx) =
  let valid = areFilesValid (map fst imp)
  in do
      lCfg  <- getLibraryCfgContents (BzoSettings imp lib flg opt pfx)
      files <- loadSourceFiles imp

      -- Load, preprocess
      fdata <- (((applyWithErrM (wrappedLibLoader lCfg)). processFiles) $ map swap files)
      -- TODO: Type Checker
      -- TODO: Static Analysis
      -- TODO: Code Generation
      putStrLn $ case valid of
                  Nothing -> showOutput $ applyWithErr orderFileData fdata
                  Just er -> show er










showOutput :: Show a => Either [BzoErr] a -> String
showOutput (Right outs) = show outs
showOutput (Left  errs) =
  "Compilation Failed. Errors:\n\n" ++ concatMap show errs









compileExpression :: (FilePath, String) -> String
compileExpression s = show (((applyWithErr wrappedParserMap). wrappedLexerMap) [swap s])
