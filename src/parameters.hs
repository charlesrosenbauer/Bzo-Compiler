module BzoParameterParser where
import Data.List
import BzoTypes
import System.IO










data SpecificFlags = Flag_OutputLLVM | Flag_OutputAssembly | Flag_FastMath
                   | Flag_PureBzo     -- | Mostly placeholders, though these are likely to still be used










data OptimizationSettings = Opt_OL1 | Opt_OL2 | Opt_OL3 | Opt_OL4 |          -- | Latency
                            Opt_OS1 | Opt_OS2 | Opt_OS3 | Opt_OS4 |          -- | Size
                            Opt_OT1 | Opt_OT2 | Opt_OT3 | Opt_OT4 | Opt_Nil  -- | Throughput, Nil










data PrefixFlags = PathFlag FilePath | GranFlag Int | OutFlag FilePath | EnvFlag FilePath










data BzoSettings
  = BzoSettings {
    importedFiles :: [(FilePath, String)],
    libraryFiles  :: [(FilePath, String)],
    flags         :: [SpecificFlags],
    optFlags      :: OptimizationSettings,
    prefixFlags   :: [PrefixFlags] }










parseFlag :: String -> Maybe String
parseFlag s = case (head s) of
  '-' -> Just s
  _   -> Nothing










parsePrefixFlag :: String -> Maybe PrefixFlags
parsePrefixFlag s
  | isPrefixOf "-p="   s = Just $ PathFlag $ drop 3 s           -- Path to search for missing files
  | isPrefixOf "-g="   s = Just $ GranFlag $ read $ drop 3 s    -- Thread Granularity (add potential failure here if input is not a valid int)
  | isPrefixOf "-o="   s = Just $ OutFlag  $ drop 3 s           -- Output Path
  | isPrefixOf "-env=" s = Just $ EnvFlag  $ drop 5 s           -- Environment Path
  | otherwise          = Nothing










addPrefixFlag :: BzoSettings -> PrefixFlags -> BzoSettings
addPrefixFlag (BzoSettings imp lib flg opt pfx) f = BzoSettings imp lib flg opt (pfx ++ [f])










parseOptimization :: String -> Maybe OptimizationSettings
parseOptimization s =
  case s of
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











tryMaybeList :: a -> [a -> Maybe b] -> Maybe b
tryMaybeList a []       = Nothing
tryMaybeList a (f : fs) =
  case (f a) of
    Just x  -> Just x
    Nothing -> tryMaybeList a fs










foldParameter :: Either BzoErr BzoSettings -> String -> Either BzoErr BzoSettings
foldParameter input par =
  case input of
    Left err -> Left err
    Right st ->
      let out = tryMaybeList st [(genericParameterParse par (parsePrefixFlag,    addPrefixFlag  )),
                                 (genericParameterParse par (parseOptimization,  addOptFlag     )),
                                 (genericParameterParse par (parseSpecificFlags, addSpecificFlag))]
      in case out of
        Nothing -> Left $ ParamErr "Invalid parameter"   -- Add more specificity
        Just x  -> Right x










parseParameters :: [String] -> Either BzoErr BzoSettings
parseParameters pars =
  let settings = BzoSettings [] [] [] Opt_Nil []
  in foldl foldParameter (Right settings) pars
