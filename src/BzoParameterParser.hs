module BzoParameterParser where
import Data.List as L
import Data.Text as T
import BzoTypes
import Error
import System.IO
import HigherOrder










readIntMaybeIter :: Maybe Int -> Char -> Maybe Int
readIntMaybeIter i c =
  case i of
    Nothing -> Nothing
    Just x  ->
      case c of
        '0' -> Just $ (x * 10)
        '1' -> Just $ (x * 10) + 1
        '2' -> Just $ (x * 10) + 2
        '3' -> Just $ (x * 10) + 3
        '4' -> Just $ (x * 10) + 4
        '5' -> Just $ (x * 10) + 5
        '6' -> Just $ (x * 10) + 6
        '7' -> Just $ (x * 10) + 7
        '8' -> Just $ (x * 10) + 8
        '9' -> Just $ (x * 10) + 9
        _   -> Nothing










readIntMaybe :: [Char] -> Maybe Int
readIntMaybe s = L.foldl readIntMaybeIter (Just 0) s










data SpecificFlags = Flag_NoFlag | Flag_HelpMePlease     -- | Mostly placeholders, though these are likely to still be used
                   deriving Show










data OptimizationSettings = Opt_Nil | Opt_None                               -- | Nil
                            deriving (Show, Eq)










data PrefixFlags
  = PathFlag {flgpath :: FilePath}
  | OutFlag  {flgpath :: FilePath}
  | EnvFlag  {flgpath :: FilePath}
  deriving Show










data BzoSettings
  = BzoSettings {
    importedFiles :: [(FilePath, String)],
    libraryFiles  :: [(FilePath, String)],
    flags         :: [SpecificFlags],
    optFlag       :: OptimizationSettings,
    prefixFlags   :: [PrefixFlags] }
    deriving Show










parseImport :: String -> Maybe String
parseImport s = case (L.head s) of
  '-' -> Nothing
  _   -> Just s










addImport :: BzoSettings -> String -> BzoSettings
addImport (BzoSettings imp lib flg opt pfx) f = BzoSettings (imp ++ [(f, "")]) lib flg opt pfx










parsePrefixFlag :: String -> Maybe PrefixFlags
parsePrefixFlag s
  | L.isPrefixOf "-p="   s = Just $ PathFlag $ L.drop 3 s           -- Path to search for missing files
  | L.isPrefixOf "-o="   s = Just $ OutFlag  $ L.drop 3 s           -- Output Path
  | L.isPrefixOf "-env=" s = Just $ EnvFlag  $ L.drop 5 s           -- Environment Path
  | otherwise              = Nothing










addPrefixFlag :: BzoSettings -> PrefixFlags -> BzoSettings
addPrefixFlag (BzoSettings imp lib flg opt pfx) f = BzoSettings imp lib flg opt (pfx ++ [f])










parseOptimization :: BzoSettings -> String -> Maybe OptimizationSettings
parseOptimization st s =
  if (optFlag st /= Opt_None)
    then Nothing
    else case s of
          _      -> Nothing










addOptFlag :: BzoSettings -> OptimizationSettings -> BzoSettings
addOptFlag (BzoSettings imp lib flg opt pfx) f = BzoSettings imp lib flg f pfx










parseSpecificFlags :: String -> Maybe SpecificFlags
parseSpecificFlags s = lookup s [("-help"           , Flag_HelpMePlease  )]










addSpecificFlag :: BzoSettings -> SpecificFlags -> BzoSettings
addSpecificFlag (BzoSettings imp lib flg opt pfx) f = BzoSettings imp lib ([f] ++ flg) opt pfx










genericParameterParse :: String -> ((String -> Maybe a), (BzoSettings -> a -> BzoSettings)) -> BzoSettings -> Maybe BzoSettings
genericParameterParse str (fa, fb) settings =
  let par = fa str
  in case par of
    Nothing -> Nothing
    Just a  -> Just $ fb settings a










foldParameter :: Either BzoErr BzoSettings -> String -> Either BzoErr BzoSettings
foldParameter input par =
  case input of
    Left err -> Left err
    Right st ->
      let out = tryMaybeList st [(genericParameterParse par (parsePrefixFlag     , addPrefixFlag  )),
                                 (genericParameterParse par (parseOptimization st, addOptFlag     )),
                                 (genericParameterParse par (parseSpecificFlags  , addSpecificFlag)),
                                 (genericParameterParse par (parseImport         , addImport      ))]
      in case out of
        Nothing -> Left $ ParamErr $ pack $ "Error on parameter " ++ (show par)
        Just x  -> Right x










parseParameters :: [String] -> Either BzoErr BzoSettings
parseParameters pars =
  let settings = BzoSettings [] [] [] Opt_None []
  in L.foldl foldParameter (Right settings) pars
