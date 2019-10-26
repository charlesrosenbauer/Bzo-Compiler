{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2019 Charles Rosenbauer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.-}

module BzoPreprocessor where
import BzoLexer
import BzoParser
import BzoParserRules
import BzoTypes
import BzoParameterParser
import BzoConfigParser
import Data.Maybe
import Data.List as L
import Data.Either
import Data.Tuple
import Data.Text as T
import Data.Map.Strict as M
import System.Directory
import System.FilePath
import Control.Monad
import System.Environment
import HigherOrder
import AST
import Error
import Core
import Tokens
import Debug.Trace
import System.IO as IO
import Control.Parallel.Strategies










processFiles :: [(FilePath, Text)] -> Either [BzoErr] [BzoFileModel BzoSyntax]
processFiles s = ((applyWithErr wrappedPrepMap). (applyWithErr wrappedParserMap). wrappedLexerMap) s










getDependencies :: BzoFileModel BzoSyntax -> [Text]
getDependencies (BzoFileModel _ _ _ _ _ l _ la) =
  let la' = L.map fst la
  in l ++ la'










setDomain :: Text -> BzoFileModel BzoSyntax -> BzoFileModel BzoSyntax
setDomain s (BzoFileModel a b _ c d e f g) = (BzoFileModel a b s c d e f g)










-- check loaded files for library dependencies, load libraries, repeat until no dependencies remain
loadLibsPass :: Map Text [FilePath] -> Map Text [BzoFileModel BzoSyntax] -> [Text] -> IO (Either [BzoErr] [BzoFileModel BzoSyntax])
loadLibsPass libs loaded [] = return $ Right $ L.concat $ elems loaded
loadLibsPass libs loaded loadme =
  let l0 = L.filter (\x -> not $ member x loaded) loadme
      l1 = L.map (\x -> case (M.lookup x libs) of
                        Just s  -> Right (x, s)
                        Nothing -> Left $ CfgErr ((pack "Could not locate library : ") `T.append` x)) l0
      l2 = lefts  l1
      (l3, l4) = unzip $ rights l1
      l5 = mapM (mapM readFile) l4
      l6 = fmap ((L.zip l4) . (L.map $ L.map pack)) l5
      l7 = fmap (L.map (\(n, fs) -> processFiles $ L.zip n fs))  l6
      l8 = fmap lefts  l7
      l9 = fmap ((L.zip l3) . rights) l7
      l9'= fmap (L.map (\(a, bs) -> (a, L.map (setDomain a) bs))) l9
      lA = fmap (insertMany loaded) l9'
      lB = fmap (L.concatMap (\(_, fd) -> L.concatMap getDependencies fd)) l9'
  in do
      l8' <- l8
      loaded' <- lA
      loadme' <- lB
      case (l2, l8') of
        ([]  , []  ) -> loadLibsPass libs loaded' loadme'   -- Now we have new dependencies. Let's load them too!
        (err0, []  ) -> return $ Left  err0
        ([]  , err1) -> return $ Left  $ L.concat err1
        (err0, err1) -> return $ Left (err0 ++ (L.concat err1))










-- load data about libraries, call loadLibsPath
loadFullProject :: FilePath -> CfgSyntax -> [BzoFileModel BzoSyntax] -> IO (Either [BzoErr] [BzoFileModel BzoSyntax])
loadFullProject path (LibLines p ls) ds =
  let path'    = (takeDirectory $ takeDirectory path) ++ "/libs/"
      libpaths = L.map (\x -> (libName x, libPath x)) ls                                                         -- format list of known libraries into list of tuples
      (l0, l1) = unzip libpaths
      l2       = mapM (getDirectoryContents . (appendFilePath path')) l1                                       -- load library file contents (just paths)
      l2'      = fmap (L.zip l1) l2
      l2''     = fmap (L.map (\(p, fs) -> L.map (\x -> path' ++ p ++ "/" ++ x) fs)) l2'                            -- get full file paths to library files
      l3       = fmap (L.map (L.filter (\x -> or[(L.isSuffixOf ".lbz" x) , (L.isSuffixOf ".bzo" x)]))) l2''         -- filter out non-source files
      l4       = fmap (\xs -> L.zip l0 xs) l3
      libpmap  = fmap (insertMany M.empty) l4                                                                    -- produce Map of library names to library contentsgit s
      loaded   = M.insert (pack "Project Files") (L.map appendStdDep ds) M.empty                                               -- produce Map of main project files
  in do
    libraryData <- libpmap
    loadLibsPass libraryData loaded (L.concatMap (getDependencies . appendStdDep) ds)

loadFullProject _ _ _ = do
  return (Left [PrepErr (BzoPos 0 0 $ pack "Full Project") $ pack "Something isn't working correctly with library loading?\n"])










wrappedLexerMap :: [(FilePath, Text)] -> Either [BzoErr] [(FilePath, [BzoToken])]
wrappedLexerMap fs =
  let contents = parMap rpar (\(f, c) -> fileLexer c f) fs
      errors   = L.concat $ lefts contents
      passes   = rights contents
      ret      = L.zip (L.map fst fs) passes
  in case errors of
      [] -> Right ret
      er -> Left  er










wrappedParserMap :: [(FilePath, [BzoToken])] -> Either [BzoErr] [BzoSyntax]
wrappedParserMap tks =
  let contents = parMap rpar (\(f, t) -> parseFile (pack f) t) tks
      errors   = L.concat $ lefts contents
      passes   = rights contents
  in case errors of
    [] -> Right passes
    er -> Left  er










wrappedPrepMap :: [BzoSyntax] -> Either [BzoErr] [BzoFileModel BzoSyntax]
wrappedPrepMap asts =
  let contents = L.map getConts asts
      errors   = lefts contents
      passes   = rights contents
  in case errors of
    [] -> Right passes
    er -> Left  er

  where getConts :: BzoSyntax -> Either BzoErr (BzoFileModel BzoSyntax)
        getConts (BzS_File ps mnam fnam inc imp    []) = Left  (PrepErr (BzoPos 0 0 fnam) $ pack "File has no defined contents.")
        getConts (BzS_File ps mnam fnam inc imp conts) = Right (BzoFileModel mnam (unpack fnam) (pack "@") (BzS_Calls (pos $ L.head conts) conts) (getIncs inc) (getImps imp) (getIncAs inc) (getImpAs imp))
        getConts bzs                                   = Left  (PrepErr (pos bzs) $ pack "File is not properly formatted.")

        getImps  :: [BzoSyntax] -> [Text]
        getImps  ((BzS_Import _ name rename):imps) = ife (name == rename) (name:(getImps imps)) (getImps imps)
        getImps  [] = []

        getImpAs :: [BzoSyntax] -> [(Text, Text)]
        getImpAs ((BzS_Import _ name rename):imps) = ife (name /= rename) ((name, rename):(getImpAs imps)) (getImpAs imps)
        getImpAs [] = []

        getIncs  :: [BzoSyntax] -> [Text]
        getIncs  ((BzS_Include _ name rename):imps) = ife (name == rename) (name:(getIncs imps)) (getIncs imps)
        getIncs  [] = []

        getIncAs :: [BzoSyntax] -> [(Text, Text)]
        getIncAs ((BzS_Include _ name rename):imps) = ife (name /= rename) ((name, rename):(getIncAs imps)) (getIncAs imps)
        getIncAs [] = []










wrappedLibLoader :: Either [BzoErr] CfgSyntax ->[BzoFileModel BzoSyntax] -> IO (Either [BzoErr] [BzoFileModel BzoSyntax])
wrappedLibLoader (Right cfg) ds = loadFullProject (unpack $ fileName $ cpos cfg) cfg ds
wrappedLibLoader (Left  err) _  = return $ Left err










loadSourceFiles :: [(FilePath, Text)] -> IO [(FilePath, Text)]
loadSourceFiles ps =
  let paths = L.map fst ps
      texts = sequence $ L.map readFile paths
  in  fmap ((L.zip paths) . (L.map pack)) texts










areFilesValid :: [FilePath] -> Maybe BzoErr
areFilesValid fs =
  case L.filter (\s -> not $ (L.isSuffixOf ".bzo" s) || (L.isSuffixOf ".lbz" s)) fs of
    [] -> Nothing
    xs -> Just $ CfgErr ((pack "The following files have invalid extensions : ") `append` (pack (show xs)))










appendFilePath :: FilePath -> FilePath -> FilePath
appendFilePath p a =
  let a' = if ((L.head a) == '/')
             then L.drop 1 a
             else a
  in if (L.last p) == '/'
       then (p ++ a)
       else (p ++ "/" ++ a)










isEnvPath :: PrefixFlags -> Bool
isEnvPath (EnvFlag s) = True
isEnvPath _           = False










getLibraryCfg :: BzoSettings -> IO (Either [BzoErr] (FilePath, Text))
getLibraryCfg (BzoSettings imp lib flg opt pfx) =
  let paths = ((L.map flgpath (L.filter isEnvPath pfx)) ++
              ["/usr/lib",
               "/usr/lib64",
               "/lib",
               "/lib64",
               "/opt/lib",
               "/opt/lib64",
               "/opt"])
      paths' = L.concatMap (\s -> [appendFilePath s "bzo/cfg/libs.cfg", appendFilePath s ".bzo/cfg/libs.cfg"]) paths
      validPaths = filterM doesFileExist paths'
  in do
    validPaths'<- validPaths
    validFiles <- fmap (\x -> sequence $ L.map readFile x) validPaths
    firstPath  <- fmap (\fs -> case fs of
                                [] -> Left [CfgErr $ pack "No valid path to a valid Bzo Environment\n"]
                                ps -> Right (L.head validPaths', pack $ L.head ps)) validFiles
    return firstPath










getLibraryCfgContents :: BzoSettings -> IO (Either [BzoErr] CfgSyntax)
getLibraryCfgContents settings =
  let text = getLibraryCfg settings
      tks  = fmap (applyWithErr (\(p, s) -> fileLexer s p)) text
  in fmap (applyWithErr $ parseLibCfgFile $ pack "libs.cfg") tks










appendStdDep :: Show a => BzoFileModel a -> BzoFileModel a
appendStdDep (BzoFileModel mn fp dmn   ast imp lnk ima lna) =
  if dmn == (pack "Std")
    then (BzoFileModel mn fp (pack "Std") ast imp lnk ima lna)
    else case (elem (pack "Std") imp, elem (pack "Std") $ L.map fst ima, elem (pack "Std") lnk, elem (pack "Std") $ L.map fst lna) of
          (False, False, False, False) -> (BzoFileModel mn fp dmn ast imp (lnk ++ [pack "Std"]) ima lna)
          (_    , _    , _    , _    ) -> (BzoFileModel mn fp dmn ast imp lnk ima lna)
