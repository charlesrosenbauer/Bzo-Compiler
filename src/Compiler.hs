module Compiler where
import Data.Tuple
import BzoTypes
import BzoPreprocessor
import BzoParameterParser
import BzoTypeModeller
import ModelRules
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
showOutput (Left [err]) =
  "Compilation Failed. Errors:\n\n" ++ show err ++ "\n\n1 error total."
showOutput (Left  errs) =
  "Compilation Failed. Errors:\n\n" ++ concatMap (\x -> show x ++ "\n\n") errs ++ "\n\n" ++ (show $ length errs) ++ " errors total."









compileExpression :: (FilePath, String) -> String
compileExpression s = showOutput (((applyWithErr wrappedModellerMapREPL). (applyWithErr wrappedParserMap). wrappedLexerMap) [swap s])










parseExpression :: (FilePath, String) -> String
parseExpression s = showOutput (((applyWithErr wrappedParserMap). wrappedLexerMap) [swap s])










lexExpression :: (FilePath, String) -> String
lexExpression s = showOutput (wrappedLexerMap [swap s])
