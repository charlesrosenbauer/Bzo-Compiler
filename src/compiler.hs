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









wrappedLexerMap :: [(FilePath, String)] -> Either [BzoErr] [(FilePath, [BzoToken])]
wrappedLexerMap fs =
  let contents = map (\(f, c) -> fileLexer f c) fs
      errors   = concat $ lefts contents
      passes   = rights contents
  in case errors of
      [] -> Right $ zip (map fst fs) passes
      er -> Left  er










wrappedParserMap :: [(FilePath, [BzoToken])] -> Either [BzoErr] [BzoSyntax]
wrappedParserMap tks =
  let contents = map (\(f, t) -> parseFile f t [parseCalls]) tks
      errors   = concat $ lefts contents
      passes   = rights contents
  in case errors of
    [] -> Right passes
    er -> Left  er










wrappedPrepMap :: [BzoSyntax] -> Either [BzoErr] [BzoFileData]
wrappedPrepMap asts =
  let contents = map (\syn -> verifyAST (Right (False, syn, (BzoFileData "" (fileName $ pos syn) syn [] [] [] [])))) asts
      errors   = concat $ lefts contents
      passes   = map (\(a, b, c) -> c) $ rights contents
  in case errors of
    [] -> Right passes
    er -> Left  er










loadSourceFiles :: [(FilePath, String)] -> IO [(FilePath, String)]
loadSourceFiles ps =
  let paths = map fst ps
      texts = sequence $ map readFile paths
  in  fmap (zip paths) texts










areFilesValid :: [FilePath] -> Maybe BzoErr
areFilesValid fs =
  case filter (\s -> not $ (isSuffixOf ".bz" s) || (isSuffixOf ".lbz" s)) fs of
    [] -> Nothing
    xs -> Just $ CfgErr ("The following files have invalid extensions : " ++ (show xs))










appendFilePath :: FilePath -> FilePath -> FilePath
appendFilePath p a =
  let a' = if ((head a) == '/')
             then drop 1 a
             else a
  in if (last p) == '/'
       then (p ++ a)
       else (p ++ "/" ++ a)










isEnvPath :: PrefixFlags -> Bool
isEnvPath (EnvFlag s) = True
isEnvPath _           = False










applyWithErr :: (b -> Either a c) -> Either a b -> Either a c
applyWithErr f x =
  case x of
    Left  a -> Left  a
    Right b -> f b










applyWithErrList :: (b -> Either a c) -> Either [a] b -> Either [a] c
applyWithErrList f x =
  case x of
    Left  a -> Left a
    Right b ->
      case f b of
        Left  a -> Left [a]
        Right b -> Right b










getLibraryCfg :: BzoSettings -> IO (Either [BzoErr] String)
getLibraryCfg (BzoSettings imp lib flg opt pfx) =
  let paths = ((map flgpath (filter isEnvPath pfx)) ++
              ["/usr/lib",
               "/urs/lib64",
               "/lib",
               "/lib64",
               "/opt/lib",
               "/opt/lib64",
               "/opt"])
      paths' = map (\s -> appendFilePath s "bzo/cfg/libs.cfg") paths
      validPaths = filterM doesFileExist paths'
  in do
    validFiles <- fmap (\x -> sequence $ map readFile x) validPaths
    firstPath  <- fmap (\fs -> case fs of
                                [] -> Left [CfgErr "No valid path to a valid Bzo Environment\n"]
                                ps -> Right$ head ps) validFiles
    return firstPath










getLibraryCfgContents :: BzoSettings -> IO (Either [BzoErr] CfgSyntax)
getLibraryCfgContents settings =
  let text = getLibraryCfg settings
      tks  = fmap (applyWithErr (\s -> fileLexer s "libs.cfg")) text
  in fmap (applyWithErr $ parseLibCfgFile "libs.cfg") tks
