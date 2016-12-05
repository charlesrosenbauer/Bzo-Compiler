module BzoLexer where
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec hiding (try, spaces)
import Data.Functor.Identity
import BzoTypes










special :: Parser Char
special = oneOf "()[]{}.,:;~@_"










symbol :: Parser Char
symbol = oneOf "`!#%^&*-+=|?<>/\\"











uppercase :: Parser Char
uppercase = oneOf "QWERTYUIOPASDFGHJKLZXCVBNM"










lowercase :: Parser Char
lowercase = oneOf "qwertyuiopasdfghjklzxcvbnm"










specialGroup :: Parser String
specialGroup = string "()"
           <|> string "[]"
           <|> string "::"
           <|> string ";;"
           <|> string ".."










spaces :: Parser ()
spaces = skipMany1 (space <|> (char '\t'))










skippableNewlines :: Parser ()
skippableNewlines = skipMany1 (char '\n')










parseString :: Parser BzoToken
parseString = do
    pos <- getPosition
    char '"'
    x <- many (noneOf "\"")
    char '"'
    many removeable
    return $ TkStr (getPos pos) x










comment :: Parser ()
comment = do
    char '\''
    x <- many (noneOf "\'")
    char '\''
    spaces










removeable :: Parser ()
removeable = skipMany1 (spaces <|> comment)










parseNewline :: Parser BzoToken
parseNewline = do
    pos <- getPosition
    char '\n'
    many (removeable <|> skippableNewlines)
    return $ TkNewline (getPos pos)










parseBuiltin :: Parser BzoToken
parseBuiltin = do
    pos <- getPosition
    char '$'
    x <- many (letter <|> digit <|> symbol)
    --many removeable
    return $ TkBuiltin (getPos pos) x










parseTypeAtom :: Parser BzoToken
parseTypeAtom = do
    pos <- getPosition
    first <- uppercase
    rest  <- many (letter <|> digit <|> symbol)
    --many removeable
    return $ TkTypeId (getPos pos) ([first] ++ rest)










parseAtom :: Parser BzoToken
parseAtom = do
    pos <- getPosition
    first <- lowercase <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    --many removeable
    return $ TkId (getPos pos) ([first] ++ rest)










parseInteger :: Parser BzoToken
parseInteger = do
    pos <- getPosition
    num <- many1 digit
    --many removeable
    return $ (TkInt (getPos pos) . read) num










parseFloat :: Parser BzoToken
parseFloat = do
    pos <- getPosition
    beg <- many1 digit
    char '.'
    end <- many1 digit
    --many removeable
    return $ (TkFlt (getPos pos) . read) (beg ++ "." ++ end)










parseSpecialGroup :: Parser BzoToken
parseSpecialGroup = do
    pos <- getPosition
    x <- specialGroup
    --many removeable
    return $ case x of
        "()" -> TkTupEmpt (getPos pos)
        "[]" -> TkArrGnrl (getPos pos)
        ".." -> TkArrMod (getPos pos)
        "::" -> TkDefine (getPos pos)
        ";;" -> TkFnSym (getPos pos)











parseSpecial :: Parser BzoToken
parseSpecial = do
    pos <- getPosition
    x <- special
    --many removeable
    return $ case x of
        '(' -> TkStartTup  (getPos pos)
        ')' -> TkEndTup    (getPos pos)
        '[' -> TkStartDat  (getPos pos)
        ']' -> TkEndDat    (getPos pos)
        '{' -> TkStartDo   (getPos pos)
        '}' -> TkEndDo     (getPos pos)
        '.' -> TkSepExpr   (getPos pos)
        ',' -> TkSepPoly   (getPos pos)
        ':' -> TkFilterSym (getPos pos)
        ';' -> TkLambdaSym (getPos pos)
        '~' -> TkMutable   (getPos pos)
        '@' -> TkReference (getPos pos)
        '_' -> TkWildcard  (getPos pos)










parseNothing :: Parser ()
parseNothing = do
  skipMany removeable










parseUnit :: Parser BzoToken
parseUnit = do
  parseNothing
  x <- parseTypeAtom
    <|> parseAtom
    <|> parseNewline
    <|> parseString
    <|> parseBuiltin
    <|> (try parseFloat)
    <|> parseInteger
    <|> (try parseSpecialGroup)
    <|> parseSpecial
  parseNothing
  return x










appendNewline :: Either ParseError [BzoToken] -> Either ParseError [BzoToken]
appendNewline x = case x of
    Left  err -> Left err
    Right tks -> Right $ tks ++ [TkNewline (BzoPos 0 0 "")]   --Position probably won't be needed from newlines










fileLexer :: String -> [Either ParseError [BzoToken]]
fileLexer s = do
    let ls  = lines s
    map (\l -> (appendNewline $ parse (many parseUnit) "Bzo REPL" l)) ls
