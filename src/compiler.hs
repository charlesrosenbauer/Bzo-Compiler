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










compileFilePass :: BzoSettings -> IO ()
compileFilePass (BzoSettings imp lib flg opt pfx) =
  let valid = areFilesValid (map fst imp)
  in do
      lCfg  <- getLibraryCfgContents (BzoSettings imp lib flg opt pfx)
      files <- loadSourceFiles imp
      fdata <- (((applyWithErrM (wrappedLibLoader lCfg)). processFiles) $ map swap files)
      -- TODO: Preprocessor
      -- TODO: Type Checker
      -- TODO: Static Analysis
      -- TODO: Code Generation
      putStrLn $ case valid of
                  Nothing -> show fdata
                  Just er -> show er










compileExpression :: (FilePath, String) -> String
compileExpression s = show (((applyWithErr wrappedParserMap). wrappedLexerMap) [swap s])
