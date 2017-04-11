module BzoParameterParser where
import Data.List










data SpecificFlags = Flag_OutputLLVM | Flag_OutputAssembly | Flag_FastMath   -- | Mostly placeholders, though these are likely to still be used










data OptimizationSettings = Opt_OL1 | Opt_OL2 | Opt_OL3 | Opt_OL4 |          -- | Latency
                            Opt_OS1 | Opt_OS2 | Opt_OS3 | Opt_OS4 |          -- | Size
                            Opt_OT1 | Opt_OT2 | Opt_OT3 | Opt_OT4 | Opt_Nil  -- | Throughput, Nil










data PrefixFlags = PathFlag FilePath | GranFlag Int | OutFlag FilePath










data BzoSettings
  = BzoSettings {
    importedFiles :: [(FilePath, String)],
    libraryFiles  :: [(FilePath, String)],
    flags         :: [SpecificFlags],
    optFlags      :: OptimizationSetting,
    prefixFlags   :: [PrefixFlags] }










parseFlag :: String -> Maybe String
parseFlag s = case (head s) of
  '-' -> Just s
  _   -> Nothing










parsePrefixFlag :: String -> Maybe PrefixFlags
parsePrefixFlag s
  | isPrefixOf "-p=" s = Just $ FilePath $ drop 3 s    -- Path to search for missing files
  | isPrefixOf "-g=" s = Just $ GranFlag $ drop 3 s    -- Thread Granularity
  | isPrefixOf "-o=" s = Just $ OutFlag  $ drop 3 s    -- Output Path
  | otherwise          = Nothing










addPrefixFlag :: BzoSettings -> PrefixFlags -> BzoSettings
addPrefixFlag (BzoSettings imp lib flg opt par pfx) f = BzoSettings imp lib flg opt par (pfx ++ [f])










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
addOptFlag (BzoSettings imp lib flg opt par pfx) f = BzoSettings imp lib flg f par pfx










parseSpecificFlags :: String -> Maybe SpecificFlags
parseSpecificFlags s = lookup [("-outputLLVM"    , Flag_OutputLLVM    ),
                               ("-outputAssembly", Flag_OutputAssembly),
                               ("-fastmath"      , Flag_FastMath      )]










addSpecificFlag :: BzoSettings -> SpecificFlags -> BzoSettings
addSpecificFlag (BzoSettings imp lib flg opt par pfx) f = BzoSettings imp lib ([f] ++ flg) opt par pfx










parseParameters :: [String] -> Either BzoErr BzoSettings
parseParameters pars =
  let settings = BzoSettings [] [] [] Opt_Nil []
  in
