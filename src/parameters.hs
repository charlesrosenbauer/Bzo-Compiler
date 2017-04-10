module BzoParameterParser where










data BzoSettings
  = BzoSettings {
    importedFiles :: [(FilePath, String)],
    linkedFiles   :: [(FilePath, String)],
    flags         :: [String],
    parFlags      :: [(String, FilePath)],
    prefixFlags   :: [(String, String)] }










parseFlag :: String -> Maybe String
parseFlag s = case (head s) of
  '-' -> Just s
  _   -> Nothing










parsePrefixFlag :: String -> Maybe (String, String)
parsePrefixFlag s = case (parseFlag s) of
  Nothing -> Nothing
  Just s' -> case (s' !! 2) of
    'l' -> Just $ drop 2 s'
    _   -> Nothing










parseParameters :: [String] -> BzoSettings
parseParameters =
