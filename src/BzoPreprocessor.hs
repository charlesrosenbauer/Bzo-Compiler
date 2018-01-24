module BzoPreprocessor where
import BzoLexer
import BzoParser
import BzoParserRules
import BzoTypes
import BzoParameterParser
import BzoConfigParser
import Data.Maybe
import Data.List hiding (map, foldl)
import Data.Either
import Data.Tuple
import Data.Map.Strict hiding (map, foldl)
import System.Directory
import System.FilePath
import Control.Monad
import System.Environment
import HigherOrder
import Debug.Trace
import System.IO hiding (try)
import Control.Parallel.Strategies










processFiles :: [(FilePath, String)] -> Either [BzoErr] [BzoFileModel BzoSyntax]
processFiles s = ((applyWithErr wrappedPrepMap). (applyWithErr wrappedParserMap). wrappedLexerMap) s










getDependencies :: BzoFileModel BzoSyntax -> [String]
getDependencies (BzoFileModel _ _ _ _ _ l _ la) =
  let la' = map fst la
  in l ++ la'










setDomain :: String -> BzoFileModel BzoSyntax -> BzoFileModel BzoSyntax
setDomain s (BzoFileModel a b _ c d e f g) = (BzoFileModel a b s c d e f g)










-- check loaded files for library dependencies, load libraries, repeat until no dependencies remain
loadLibsPass :: Map String [FilePath] -> Map String [BzoFileModel BzoSyntax] -> [String] -> IO (Either [BzoErr] [BzoFileModel BzoSyntax])
loadLibsPass libs loaded [] = return $ Right $ concat $ elems loaded
loadLibsPass libs loaded loadme =
  let l0 = Prelude.filter (\x -> not $ member x loaded) loadme
      l1 = map (\x -> case (Data.Map.Strict.lookup x libs) of
                        Just s  -> Right (x, s)
                        Nothing -> Left $ CfgErr ("Could not locate library : " ++ x)) l0
      l2 = lefts  l1
      (l3, l4) = unzip $ rights l1
      l5 = mapM (mapM readFile) l4
      l6 = fmap (zip l3) l5
      l7 = fmap (map (\(n, fs) -> processFiles $ zip fs (repeat n))) l6
      l8 = fmap lefts  l7
      l9 = fmap ((zip l3) . rights) l7
      l9'= fmap (map (\(a, bs) -> (a, map (setDomain a) bs))) l9
      lA = fmap (insertMany loaded) l9'
      lB = fmap (concatMap (\(_, fd) -> concatMap getDependencies fd)) l9'
  in do
      l8' <- l8
      loaded' <- lA
      loadme' <- lB
      case (l2, l8') of
        ([]  , []  ) -> loadLibsPass libs loaded' loadme'   -- Now we have new dependencies. Let's load them too!
        (err0, []  ) -> return $ Left  err0
        ([]  , err1) -> return $ Left  $ concat err1
        (err0, err1) -> return $ Left (err0 ++ (concat err1))










-- load data about libraries, call loadLibsPath
loadFullProject :: FilePath -> CfgSyntax -> [BzoFileModel BzoSyntax] -> IO (Either [BzoErr] [BzoFileModel BzoSyntax])
loadFullProject path (LibLines p ls) ds =
  let path'    = (takeDirectory $ takeDirectory path) ++ "/libs/"
      libpaths = map (\x -> (libName x, libPath x)) ls                                                         -- format list of known libraries into list of tuples
      (l0, l1) = unzip libpaths
      l2       = mapM (getDirectoryContents . (appendFilePath path')) l1                                       -- load library file contents (just paths)
      l2'      = fmap (zip l1) l2
      l2''     = fmap (map (\(p, fs) -> map (\x -> path' ++ p ++ "/" ++ x) fs)) l2'                            -- get full file paths to library files
      l3       = fmap (map (Prelude.filter (\x -> or[(isSuffixOf ".lbz" x) , (isSuffixOf ".bz" x)]))) l2''     -- filter out non-source files
      l4       = fmap (zip l0) l3
      libpmap  = fmap (insertMany empty) l4                                                                    -- produce Map of library names to library contentsgit s
      loaded   = Data.Map.Strict.insert "Project Files" (map appendStdDep ds) empty                                               -- produce Map of main project files
  in do
    libraryData <- libpmap
    loadLibsPass libraryData loaded (concatMap (getDependencies . appendStdDep) ds)

loadFullProject _ _ _ = do
  return (Left [PrepErr (BzoPos 0 0 "Full Project") "Something isn't working correctly with library loading?\n"])










wrappedLexerMap :: [(FilePath, String)] -> Either [BzoErr] [(FilePath, [BzoToken])]
wrappedLexerMap fs =
  let contents = parMap rpar (\(f, c) -> fileLexer c f) fs
      errors   = concat $ lefts contents
      passes   = rights contents
      ret      = zip (Prelude.map fst fs) passes
  in case errors of
      [] -> Right ret
      er -> Left  er










wrappedParserMap :: [(FilePath, [BzoToken])] -> Either [BzoErr] [BzoSyntax]
wrappedParserMap tks =
  let contents = parMap rpar (\(f, t) -> parseFile f t) tks
      errors   = concat $ lefts contents
      passes   = rights contents
  in case errors of
    [] -> Right passes
    er -> Left  er










wrappedPrepMap :: [BzoSyntax] -> Either [BzoErr] [BzoFileModel BzoSyntax]
wrappedPrepMap asts =
  let contents = map getConts asts
      errors   = lefts contents
      passes   = rights contents
  in case errors of
    [] -> Right passes
    er -> Left  er

  where getConts :: BzoSyntax -> Either BzoErr (BzoFileModel BzoSyntax)
        getConts (BzS_File ps mnam fnam inc imp conts) = Right (BzoFileModel mnam fnam "@" (BzS_Calls (pos $ head conts) conts) (getImps imp) (getIncs inc) (getImpAs imp) (getIncAs inc))
        getConts bzs                                   = Left  (PrepErr (pos bzs) "File is not properly formatted.")

        getImps  :: [BzoSyntax] -> [String]
        getImps  ((BzS_Import _ name rename):imps) = ife (name == rename) (name:(getImps imps)) (getImps imps)
        getImps  [] = []

        getImpAs :: [BzoSyntax] -> [(String, String)]
        getImpAs ((BzS_Import _ name rename):imps) = ife (name /= rename) ((name, rename):(getImpAs imps)) (getImpAs imps)
        getImpAs [] = []

        getIncs  :: [BzoSyntax] -> [String]
        getIncs  ((BzS_Include _ name rename):imps) = ife (name == rename) (name:(getIncs imps)) (getIncs imps)
        getIncs  [] = []

        getIncAs :: [BzoSyntax] -> [(String, String)]
        getIncAs ((BzS_Include _ name rename):imps) = ife (name /= rename) ((name, rename):(getIncAs imps)) (getIncAs imps)
        getIncAs [] = []










wrappedLibLoader :: Either [BzoErr] CfgSyntax ->[BzoFileModel BzoSyntax] -> IO (Either [BzoErr] [BzoFileModel BzoSyntax])
wrappedLibLoader (Right cfg) ds = loadFullProject (fileName $ cpos cfg) cfg ds
wrappedLibLoader (Left  err) _  = return $ Left err










loadSourceFiles :: [(FilePath, String)] -> IO [(FilePath, String)]
loadSourceFiles ps =
  let paths = Prelude.map fst ps
      texts = sequence $ Prelude.map readFile paths
  in  fmap (zip paths) texts










areFilesValid :: [FilePath] -> Maybe BzoErr
areFilesValid fs =
  case Prelude.filter (\s -> not $ (isSuffixOf ".bz" s) || (isSuffixOf ".lbz" s)) fs of
    [] -> Nothing
    xs -> Just $ CfgErr ("The following files have invalid extensions : " ++ (show xs))










appendFilePath :: FilePath -> FilePath -> FilePath
appendFilePath p a =
  let a' = if ((head a) == '/')
             then Data.List.drop 1 a
             else a
  in if (last p) == '/'
       then (p ++ a)
       else (p ++ "/" ++ a)










isEnvPath :: PrefixFlags -> Bool
isEnvPath (EnvFlag s) = True
isEnvPath _           = False










getLibraryCfg :: BzoSettings -> IO (Either [BzoErr] (FilePath, String))
getLibraryCfg (BzoSettings imp lib flg opt pfx) =
  let paths = ((Prelude.map flgpath (Prelude.filter isEnvPath pfx)) ++
              ["/usr/lib",
               "/usr/lib64",
               "/lib",
               "/lib64",
               "/opt/lib",
               "/opt/lib64",
               "/opt"])
      paths' = Prelude.concatMap (\s -> [appendFilePath s "bzo/cfg/libs.cfg", appendFilePath s ".bzo/cfg/libs.cfg"]) paths
      validPaths = filterM doesFileExist paths'
  in do
    validPaths'<- validPaths
    validFiles <- fmap (\x -> sequence $ Prelude.map readFile x) validPaths
    firstPath  <- fmap (\fs -> case fs of
                                [] -> Left [CfgErr "No valid path to a valid Bzo Environment\n"]
                                ps -> Right (head validPaths', head ps)) validFiles
    return firstPath









{-
getLibraryCfgContents :: BzoSettings -> IO (Either [BzoErr] CfgSyntax)
getLibraryCfgContents settings =
  let text = getLibraryCfg settings
      tks  = fmap (applyWithErr (\(p, s) -> fileLexer s p)) text
  in fmap (applyWithErr $ parseLibCfgFile "libs.cfg") tks-}










appendStdDep :: Show a => BzoFileModel a -> BzoFileModel a
appendStdDep (BzoFileModel mn fp "Std" ast imp lnk ima lna) = (BzoFileModel mn fp "Std" ast imp lnk ima lna)
appendStdDep (BzoFileModel mn fp dmn   ast imp lnk ima lna) =
  case (elem "Std" imp, elem "Std" $ map fst ima, elem "Std" lnk, elem "Std" $ map fst lna) of
    (False, False, False, False) -> (BzoFileModel mn fp dmn ast imp (lnk ++ ["Std"]) ima lna)
    (_    , _    , _    , _    ) -> (BzoFileModel mn fp dmn ast imp lnk ima lna)
