module Compiler where
import System.IO
import System.Directory
import Control.Monad
import Data.Maybe
import BzoParser
import BzoParserRules
import BzoTypes
import BzoLexer
import BzoSyntax
import BzoTokens
import BzoParameterParser
import BzoConfigParser










compileFile :: String -> String -> String
compileFile name f =
  let out = fileLexer f name
  in  case out of
    Left  err -> show err
    Right tks ->
      show $ parseFile name tks [parseCalls]










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
                                [] -> Left [PrepErr "No valid path to a valid Bzo Environment\n"]
                                ps -> Right$ head ps) validFiles
    return firstPath










getLibraryCfgContents :: BzoSettings -> IO (Either [BzoErr] CfgSyntax)
getLibraryCfgContents settings =
  let text = getLibraryCfg settings
      tks  = fmap (applyWithErr (\s -> fileLexer s "libs.cfg")) text
  in fmap (applyWithErr $ parseLibCfgFile "libs.cfg") tks
