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
import Debug.Trace










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










filterMaybe :: [a] -> (a -> Maybe b) -> [b]
filterMaybe xs f = catMaybes $ filter isJust $ map f xs










getLibraryCfg :: BzoSettings -> IO (Either BzoErr String)
getLibraryCfg (BzoSettings imp lib flg opt pfx) =
  let paths = ["/usr/lib",
               "/urs/lib64",
               "/lib",
               "/lib64",
               "/opt/lib",
               "/opt/lib64",
               "/opt"] ++ (map flgpath (filter isEnvPath pfx))
      paths' = map (\s -> appendFilePath s "/bzo/cfg/libs.cfg") paths
      validPaths = filterM doesFileExist paths'
  in do
    validFiles <- fmap (\x -> sequence $ map readFile x) validPaths
    firstPath  <- fmap (\fs -> case fs of
                                [] -> Left $ PrepErr "No valid path to a valid Bzo Environment\n"
                                ps -> Right$ head ps) validFiles
    return $ firstPath
