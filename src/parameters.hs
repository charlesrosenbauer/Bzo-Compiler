module BzoParameterParser where
import Data.List
import BzoTypes
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
readIntMaybe s = foldl readIntMaybeIter (Just 0) s










data SpecificFlags = Flag_OutputLLVM | Flag_OutputAssembly | Flag_FastMath
                   | Flag_PureBzo     -- | Mostly placeholders, though these are likely to still be used
                   deriving Show










data OptimizationSettings = Opt_OL1 | Opt_OL2 | Opt_OL3 | Opt_OL4 |          -- | Latency
                            Opt_OS1 | Opt_OS2 | Opt_OS3 | Opt_OS4 |          -- | Size
                            Opt_OT1 | Opt_OT2 | Opt_OT3 | Opt_OT4 |          -- | Throughput
                            Opt_Nil | Opt_None                               -- | Nil
                            deriving (Show, Eq)










data PrefixFlags
  = PathFlag {flgpath :: FilePath}
  | GranFlag Int
  | OutFlag {flgpath :: FilePath}
  | EnvFlag {flgpath :: FilePath}
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
parseImport s = case (head s) of
  '-' -> Nothing
  _   -> Just s










addImport :: BzoSettings -> String -> BzoSettings
addImport (BzoSettings imp lib flg opt pfx) f = BzoSettings (imp ++ [(f, "")]) lib flg opt pfx










parsePrefixFlag :: String -> Maybe PrefixFlags
parsePrefixFlag s
  | isPrefixOf "-p="   s = Just $ PathFlag $ drop 3 s           -- Path to search for missing files
  | isPrefixOf "-o="   s = Just $ OutFlag  $ drop 3 s           -- Output Path
  | isPrefixOf "-env=" s = Just $ EnvFlag  $ drop 5 s           -- Environment Path
  | isPrefixOf "-g="   s =
      case (readIntMaybe $ drop 3 s) of
        Just x  -> Just $ GranFlag x    -- Thread Granularity (add potential failure here if input is not a valid int)
        Nothing -> Nothing
  | otherwise          = Nothing










addPrefixFlag :: BzoSettings -> PrefixFlags -> BzoSettings
addPrefixFlag (BzoSettings imp lib flg opt pfx) f = BzoSettings imp lib flg opt (pfx ++ [f])










parseOptimization :: BzoSettings -> String -> Maybe OptimizationSettings
parseOptimization st s =
  if (optFlag st /= Opt_None)
    then Nothing
    else case s of
          "-O"   -> Just Opt_OT2
          "-O0"  -> Just Opt_Nil
          "-O1"  -> Just Opt_OT1
          "-O2"  -> Just Opt_OT2
          "-O3"  -> Just Opt_OT3
          "-O4"  -> Just Opt_OT4
          "-OT1" -> Just Opt_OT1
          "-OT2" -> Just Opt_OT2
          "-OT3" -> Just Opt_OT3
          "-OT4" -> Just Opt_OT4
          "-OS1" -> Just Opt_OS1
          "-OS2" -> Just Opt_OS2
          "-OS3" -> Just Opt_OS3
          "-OS4" -> Just Opt_OS4
          "-OL1" -> Just Opt_OL1
          "-OL2" -> Just Opt_OL2
          "-OL3" -> Just Opt_OL3
          "-OL4" -> Just Opt_OL4
          _      -> Nothing










addOptFlag :: BzoSettings -> OptimizationSettings -> BzoSettings
addOptFlag (BzoSettings imp lib flg opt pfx) f = BzoSettings imp lib flg f pfx










parseSpecificFlags :: String -> Maybe SpecificFlags
parseSpecificFlags s = lookup s [("-output-llvm"    , Flag_OutputLLVM    ),
                                 ("-output-assembly", Flag_OutputAssembly),
                                 ("-fastmath"       , Flag_FastMath      ),
                                 ("-pure-bzo"       , Flag_PureBzo       )]










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
        Nothing -> Left $ ParamErr $ "Error on parameter " ++ (show par)
        Just x  -> Right x










parseParameters :: [String] -> Either BzoErr BzoSettings
parseParameters pars =
  let settings = BzoSettings [] [] [] Opt_None []
  in foldl foldParameter (Right settings) pars
