module Compiler where
import System.IO
import System.Directory
import Control.Monad
import Data.Maybe
import Data.List
import Data.Either
import Data.Tuple
import BzoParser
import BzoParserRules
import BzoTypes
import BzoLexer
import BzoSyntax
import BzoTokens
import BzoParameterParser
import BzoConfigParser
import BzoPreprocessor










compileFilePass :: BzoSettings -> IO ()
compileFilePass (BzoSettings imp lib flg opt pfx) =
  let valid = areFilesValid (map fst imp)
  in do
      lCfg  <- getLibraryCfgContents (BzoSettings imp lib flg opt pfx)
      files <- loadSourceFiles imp
      -- TODO: Preprocessor
      -- TODO: Type Checker
      -- TODO: Static Analysis
      -- TODO: Code Generation
      putStrLn $ case valid of
                  Nothing -> show (((applyWithErr wrappedPrepMap). (applyWithErr wrappedParserMap). wrappedLexerMap) $ map swap files)
                  Just er -> show er










compileExpression :: (FilePath, String) -> String
compileExpression s = show (((applyWithErr wrappedParserMap). wrappedLexerMap) [swap s])
