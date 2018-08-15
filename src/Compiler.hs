module Compiler where
import Data.Tuple
import Data.List
import BzoTypes
import BzoPreprocessor
import BzoParameterParser
import BzoFileSorter
import ModelRules
import DefinitionTable
import BzoChecker
import HigherOrder
import Debug.Trace










compileFilePass :: BzoSettings -> IO ()
compileFilePass (BzoSettings imp lib flg opt pfx) =
  let valid = areFilesValid (map fst imp)
  in do
      lCfg  <- getLibraryCfgContents (BzoSettings imp lib flg opt pfx)
      files <- loadSourceFiles imp

      -- Load, preprocess

      defs       <- return $ processFiles files
      defs'      <- (fmap (applyWithErr orderFileData)) $ (applyWithErrM (wrappedLibLoader lCfg)) defs
      defs''     <- return $ (applyRight getDefTable) $ (applyRight getDefs) $ (applyRight (map (\x -> (adjustModel x modelXForm)))) defs'
      --models   <- (((fmap (applyWithErr orderFileData)). (fmap $ applyWithErr wrappedModellerMap). (applyWithErrM (wrappedLibLoader lCfg)). processFiles) $ map swap files)
      --symbols  <- return (applyRight generateSymbolTable models)
      --namemaps <- return (applyRight (\st -> applyRight (map (getNamespaces st)) models) symbols)
      --defs     <- return $ (applyWithErr wrappedDefOrganizePass) models
      --types    <- return (wrappedGenerateTypes symbols defs)
      -- TODO: Type Checker
      -- TODO: Static Analysis
      -- TODO: Code Generation
      putStrLn $ case valid of
                  Nothing -> showOutput {-$ trace (show namemaps)-} defs''
                  Just er -> show er










showOutput :: Show a => Either [BzoErr] a -> String
showOutput (Right outs) = show outs
showOutput (Left [err]) =
  "Compilation Failed. Errors:\n\n" ++ show err ++ "\n\n1 error total."
showOutput (Left  errs) =
  "Compilation Failed. Errors:\n\n" ++ concatMap (\x -> show x ++ "\n\n") (sortErrs $ errs) ++ "\n\n" ++ (show $ length errs) ++ " errors total."










sortErrs :: [BzoErr] -> [BzoErr]
sortErrs = sortBy (\a b->
  case (compare (fileName $ position a) (fileName $ position b)) of
    EQ -> case (compare (line $ position a) (line $ position b)) of
            EQ -> (compare (column $ position a) (column $ position b))
            x  -> x
    x  -> x )









--compileExpression :: (FilePath, String) -> Either [BzoErr] [BzoFileModel]
--compileExpression lCfg s = (((applyWithErrM (wrappedLibLoader lCfg)). (applyWithErr wrappedModellerMap). (applyWithErr wrappedParserMap). wrappedLexerMap) [swap s])









replExpression :: (FilePath, String) -> String
replExpression s = showOutput (({-}(applyWithErr wrappedModellerMapREPL).-} (applyWithErr wrappedParserMap). wrappedLexerMap) [s])










parseExpression :: (FilePath, String) -> String
parseExpression s = showOutput (((applyWithErr wrappedParserMap). wrappedLexerMap) [swap s])










lexExpression :: (FilePath, String) -> String
lexExpression s = showOutput (wrappedLexerMap [swap s])
