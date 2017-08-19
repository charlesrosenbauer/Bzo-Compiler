module BzoPreprocessor where
import BzoLexer
import BzoParser
import BzoParserRules
import BzoSyntax
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










data BzoFileData
  = BzoFileData{
      bfd_moduleName    :: String,
      bfd_filepath      :: FilePath,
      bfd_domain        :: String,
      bfd_fileAST       :: BzoSyntax,
      bfd_fileImports   :: [String],
      bfd_fileLinks     :: [String],
      bfd_fileImporsAs  :: [(String, String)],
      bfd_fileLinksAs   :: [(String, String)]}










showBzoFileData :: BzoFileData -> String
showBzoFileData (BzoFileData mn pth dmn ast imp lnk impa lnka) =
  "\n \nModule: " ++ mn ++ "\nPath: " ++ pth ++
    "\nDomain: "  ++ (show dmn) ++
    "\nImports: " ++ (show imp) ++ "\nAliased Imports: " ++ (show impa) ++
    "\nLinks: "   ++ (show lnk) ++ "\nAliased Links: "   ++ (show lnka) ++
    "\nAST:\n"     ++ (show ast)
instance Show BzoFileData where show = showBzoFileData










data BzoPrepPattern
  = PatStr String
  | PatInt Integer
  | PatFlt Double
  | PatId  String
  | PatMId String
  | PatTId String
  | PatBId String
  | PatBTy String
  | PatCmp [BzoPrepPattern]
  | PatPly [BzoPrepPattern]










mkPat_Str = PatStr ""
mkPat_Int = PatInt 0
mkPat_Flt = PatFlt 0.0
mkPat_Id  = PatId  ""
mkPat_MId = PatMId ""
mkPat_TId = PatTId ""
mkPat_BId = PatBId ""
mkPat_BTy = PatBTy ""
mkPat_Cmp = PatCmp []
mkPat_Ply = PatPly []










matchPattern :: BzoPrepPattern -> BzoSyntax -> Bool
matchPattern (PatStr s) (BzS_Str   p s') = True
matchPattern (PatInt i) (BzS_Int   p i') = True
matchPattern (PatFlt f) (BzS_Flt   p f') = True
matchPattern (PatId  i) (BzS_Id    p i') = True
matchPattern (PatMId i) (BzS_MId   p i') = True
matchPattern (PatTId i) (BzS_TyId  p i') = True
matchPattern (PatBId i) (BzS_BId   p i') = True
matchPattern (PatBTy i) (BzS_BTId  p i') = True
matchPattern (PatCmp x) (BzS_Cmpd  p x') = True
matchPattern (PatPly x) (BzS_Poly  p x') = True
matchPattern _          _                = False










matchBCall0 :: String -> BzoSyntax -> Bool
matchBCall0 s expr =
  case expr of
    (BzS_Expr p [(BzS_BTId p' st)]) -> (st == s)
    (BzS_Expr p [(BzS_BId  p' st)]) -> (st == s)
    _                               -> False










matchBCall1 :: String -> BzoPrepPattern -> BzoSyntax -> Maybe BzoSyntax
matchBCall1 s p0 expr =
  case expr of
    (BzS_Expr p [x0, (BzS_BTId p' st)]) -> maybeIf ((st == s) && (matchPattern p0 x0)) x0
    (BzS_Expr p [x0, (BzS_BId  p' st)]) -> maybeIf ((st == s) && (matchPattern p0 x0)) x0
    _                                   -> Nothing










matchBCall2 :: String -> BzoSyntax -> BzoPrepPattern -> Maybe BzoSyntax
matchBCall2 s expr p1 =
  case expr of
    (BzS_Expr p [(BzS_BTId p' st), x1]) -> maybeIf ((st == s) && (matchPattern p1 x1)) x1
    (BzS_Expr p [(BzS_BId  p' st), x1]) -> maybeIf ((st == s) && (matchPattern p1 x1)) x1
    _                                   -> Nothing










matchBCall3 :: String -> BzoPrepPattern -> BzoSyntax -> BzoPrepPattern -> Maybe (BzoSyntax, BzoSyntax)
matchBCall3 s p0 expr p1 =
  case expr of
    (BzS_Expr p [x0, (BzS_BTId p' st), x1]) -> maybeIf ((st == s) && (matchPattern p0 x0) && (matchPattern p1 x1)) (x0, x1)
    (BzS_Expr p [x0, (BzS_BId  p' st), x1]) -> maybeIf ((st == s) && (matchPattern p0 x0) && (matchPattern p1 x1)) (x0, x1)
    _                                   -> Nothing










verifyAST :: Either [BzoErr] (Bool, BzoSyntax, BzoFileData) -> Either [BzoErr] (Bool, BzoSyntax, BzoFileData)
verifyAST (Left errs) = Left errs




verifyAST (Right (b,     (BzS_Calls p []),     (BzoFileData mn path dmn ast imp lnk impa lnka))) = Right (b, ast, (BzoFileData mn path dmn ast imp lnk impa lnka))




verifyAST (Right (False, ast, (BzoFileData "" path dmn (BzS_Calls p (x:xs)) [] [] [] []))) =
      case (matchBCall1 "$Module" mkPat_TId x) of
        Just syn -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData (sid syn) path dmn (BzS_Calls p xs) [] [] [] []))
        _        -> Left [PrepErr p "Illegal formatting. $Module must be defined at beginning of file.\n"]




verifyAST (Right (False, ast, (BzoFileData mn path dmn (BzS_Calls p (x : xs)) imp lnk impa lnka))) =
      case (matchBCall1 "$import" mkPat_TId x) of
        Just syn -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData mn path dmn (BzS_Calls p xs) ([sid syn] ++ imp) lnk impa lnka))
        _        ->
          case (matchBCall1 "$link" mkPat_TId x) of
            Just syn -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData mn path dmn (BzS_Calls p xs) imp ([sid syn] ++ lnk) impa lnka))
            _        ->
              case (matchBCall3 "$importAs" mkPat_TId x mkPat_TId) of
                Just (a, b) -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData mn path dmn (BzS_Calls p xs) imp lnk ([(sid a, sid b)] ++ impa) lnka))
                _           ->
                  case (matchBCall3 "$linkAs" mkPat_TId x mkPat_TId) of
                    Just (a, b) -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData mn path dmn (BzS_Calls p xs) imp lnk impa ([(sid a, sid b)] ++ lnka)))
                    _           -> verifyAST $ Right (True,  ast,              (BzoFileData mn path dmn (BzS_Calls p (x : xs)) imp lnk impa lnka))




verifyAST (Right (True, (BzS_Calls p (x : xs)), (BzoFileData mn path dmn ast imp lnk impa lnka))) =
      case (matchBCall1 "$import" mkPat_TId x) of
        Just syn -> Left [PrepErr (pos x) "Illegal formatting. All instances of $import must be at the beginning of file.\n"]
        _        ->
          case (matchBCall1 "$link" mkPat_TId x) of
            Just syn -> Left [PrepErr (pos x) "Illegal formatting. All instances of $link must be at the beginning of file.\n"]
            _        ->
              case (matchBCall3 "$importAs" mkPat_TId x mkPat_TId) of
                Just syn -> Left [PrepErr (pos x) "Illegal formatting. All instances of $importAs must be at the beginning of file.\n"]
                _        ->
                  case (matchBCall3 "$linkAs" mkPat_TId x mkPat_TId) of
                    Just syn -> Left [PrepErr (pos x) "Illegal formatting. All instances of $linkAs must be at the beginning of file.\n"]
                    _        ->
                      case (matchBCall1 "$Module" mkPat_TId x) of
                        Just syn -> Left [PrepErr (pos x) "Illegal formatting. Only one instance of $Module per file.\n"]
                        _        -> verifyAST $ Right (True, (BzS_Calls p xs), (BzoFileData mn path dmn ast imp lnk impa lnka))










processFiles :: [(FilePath, String)] -> Either [BzoErr] [BzoFileData]
processFiles s = ((applyWithErr wrappedPrepMap). (applyWithErr wrappedParserMap). wrappedLexerMap) s










getDependencies :: BzoFileData -> [String]
getDependencies (BzoFileData _ _ _ _ _ l _ la) =
  let la' = map fst la
  in l ++ la'










setDomain :: String -> BzoFileData -> BzoFileData
setDomain s (BzoFileData a b _ c d e f g) = (BzoFileData a b s c d e f g)










-- check loaded files for library dependencies, load libraries, repeat until no dependencies remain
loadLibsPass :: Map String [FilePath] -> Map String [BzoFileData] -> [String] -> IO (Either [BzoErr] [BzoFileData])
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
loadFullProject :: FilePath -> CfgSyntax -> [BzoFileData] -> IO (Either [BzoErr] [BzoFileData])
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
      loaded   = Data.Map.Strict.insert "Project Files" ds empty                                               -- produce Map of main project files
  in do
    libraryData <- libpmap
    loadLibsPass libraryData loaded (concatMap getDependencies ds)

loadFullProject _ _ _ = do
  return (Left [PrepErr (BzoPos 0 0 "Full Project") "Something isn't working correctly with library loading?\n"])










wrappedLexerMap :: [(FilePath, String)] -> Either [BzoErr] [(FilePath, [BzoToken])]
wrappedLexerMap fs =
  let contents = Prelude.map (\(f, c) -> fileLexer f c) fs
      errors   = concat $ lefts contents
      passes   = rights contents
  in case errors of
      [] -> Right $ zip (Prelude.map fst fs) passes
      er -> Left  er










wrappedParserMap :: [(FilePath, [BzoToken])] -> Either [BzoErr] [BzoSyntax]
wrappedParserMap tks =
  let contents = Prelude.map (\(f, t) -> parseFile f t [parseCalls]) tks
      errors   = concat $ lefts contents
      passes   = rights contents
  in case errors of
    [] -> Right passes
    er -> Left  er










wrappedPrepMap :: [BzoSyntax] -> Either [BzoErr] [BzoFileData]
wrappedPrepMap asts =
  let contents = Prelude.map (\syn -> verifyAST (Right (False, syn, (BzoFileData "" (fileName $ pos syn) "@" syn [] [] [] [])))) asts
      errors   = concat $ lefts contents
      passes   = Prelude.map (\(a, b, c) -> c) $ rights contents
  in case errors of
    [] -> Right passes
    er -> Left  er










wrappedLibLoader :: Either [BzoErr] CfgSyntax ->[BzoFileData] -> IO (Either [BzoErr] [BzoFileData])
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
             then drop 1 a
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










getLibraryCfgContents :: BzoSettings -> IO (Either [BzoErr] CfgSyntax)
getLibraryCfgContents settings =
  let text = getLibraryCfg settings
      tks  = fmap (applyWithErr (\(p, s) -> fileLexer s p)) text
  in fmap (applyWithErr $ parseLibCfgFile "libs.cfg") tks