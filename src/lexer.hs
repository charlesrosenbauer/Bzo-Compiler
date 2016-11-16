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
           <|> string "{}"
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
    return $ TkStr pos x










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
    --pos <- getPosition
    many1 (char '\n')
    many (removeable <|> skippableNewlines)
    return $ TkNewline










parseBuiltin :: Parser BzoToken
parseBuiltin = do
    pos <- getPosition
    char '$'
    x <- many (letter <|> digit <|> symbol)
    many removeable
    return $ TkBuiltin pos x










parseTypeAtom :: Parser BzoToken
parseTypeAtom = do
    pos <- getPosition
    first <- uppercase
    rest  <- many (letter <|> digit <|> symbol)
    many removeable
    return $ TkTypeId pos ([first] ++ rest)










parseAtom :: Parser BzoToken
parseAtom = do pos <- getPosition
               first <- lowercase <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               many removeable
               return $ TkId pos ([first] ++ rest)










parseInteger :: Parser BzoToken
parseInteger = do
    pos <- getPosition
    num <- many1 digit
    many removeable
    return $ (TkInt pos . read) num










parseFloat :: Parser BzoToken
parseFloat = do
    pos <- getPosition
    beg <- many1 digit
    char '.'
    end <- many1 digit
    many removeable
    return $ (TkFlt pos . read) (beg ++ "." ++ end)










parseSpecialGroup :: Parser BzoToken
parseSpecialGroup = do
    pos <- getPosition
    x <- specialGroup
    many removeable
    return $ case x of
        "()" -> TkTupEmpt pos
        "[]" -> TkArrGnrl pos
        "{}" -> TkExpGnrl pos
        ".." -> TkArrMod pos
        "::" -> TkDefine pos
        ";;" -> TkFnSym pos
                           










parseSpecial :: Parser BzoToken
parseSpecial = do
    pos <- getPosition
    x <- special
    many removeable
    return $ case x of
        '(' -> TkStartTup pos
        ')' -> TkEndTup pos
        '[' -> TkStartDat pos
        ']' -> TkEndDat pos
        '{' -> TkStartDo pos
        '}' -> TkEndDo pos
        '.' -> TkSepExpr pos
        ',' -> TkSepPoly pos
        ':' -> TkFilterSym pos
        ';' -> TkLambdaSym pos
        '~' -> TkMutable pos
        '@' -> TkReference pos
        '_' -> TkWildcard pos










parseUnit :: Parser BzoToken
parseUnit = parseTypeAtom
        <|> parseAtom
        <|> parseNewline
        <|> parseString
        <|> parseBuiltin
        <|> (try parseFloat)
        <|> parseInteger
        <|> (try parseSpecialGroup)
        <|> parseSpecial










appendNewline :: Either ParseError [BzoToken] -> Either ParseError [BzoToken]
appendNewline x = case x of
    Left  err -> Left err
    Right tks -> Right $ tks ++ [TkNewline]










fileLexer :: String -> [Either ParseError [BzoToken]]
fileLexer s = do
    let ls  = lines s
    map (\l -> (appendNewline $ parse (many parseUnit) "Bzo" l)) ls
                          

































