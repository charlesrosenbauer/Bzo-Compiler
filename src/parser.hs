module BzoParser where
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










parseString :: Parser Token
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










parseNewline :: Parser Token
parseNewline = do
    pos <- getPosition
    many1 (char '\n')
    many (removeable <|> skippableNewlines)
    return $ TkNewline pos










parseBuiltin :: Parser Token
parseBuiltin = do
    pos <- getPosition
    char '$'
    x <- many (letter <|> digit <|> symbol)
    many removeable
    return $ TkBuiltin pos x










parseTypeAtom :: Parser Token
parseTypeAtom = do
    pos <- getPosition
    first <- uppercase
    rest  <- many (letter <|> digit <|> symbol)
    many removeable
    return $ TkTypeId pos ([first] ++ rest)










parseAtom :: Parser Token
parseAtom = do pos <- getPosition
               first <- lowercase <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               many removeable
               return $ TkId pos ([first] ++ rest)










parseInteger :: Parser Token
parseInteger = do
    pos <- getPosition
    num <- many1 digit
    many removeable
    return $ (TkInt pos . read) num










parseFloat :: Parser Token
parseFloat = do
    pos <- getPosition
    beg <- many1 digit
    char '.'
    end <- many1 digit
    many removeable
    return $ (TkFlt pos . read) (beg ++ "." ++ end)










parseSpecialGroup :: Parser Token
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
                           










parseSpecial :: Parser Token
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










parseUnit :: Parser Token
parseUnit = parseTypeAtom
        <|> parseAtom
        <|> parseNewline
        <|> parseString
        <|> parseBuiltin
        <|> (try parseFloat)
        <|> parseInteger
        <|> (try parseSpecialGroup)
        <|> parseSpecial










parseExpr :: Parser [Token]
parseExpr = many parseUnit





















showTk :: Token -> String
showTk (TkStartTup      _) = "("
showTk (TkEndTup        _) = ")"
showTk (TkStartDat      _) = "["
showTk (TkEndDat        _) = "]"
showTk (TkSepExpr       _) = "."
showTk (TkSepPoly       _) = ","
showTk (TkFilterSym     _) = ":"
showTk (TkLambdaSym     _) = ";"
showTk (TkMutable       _) = "~"
showTk (TkReference     _) = "@"
showTk (TkWildcard      _) = "_"
showTk (TkDefine        _) = "::"
showTk (TkFnSym         _) = ";;"
showTk (TkTupEmpt       _) = ")"
showTk (TkArrGnrl       _) = "[]"
showTk (TkExpGnrl       _) = "{}"
showTk (TkArrMod        _) = ".."
showTk (TkInt        _  x) = "I:"   ++ show x
showTk (TkFlt        _  x) = "F:"   ++ show x
showTk (TkStr        _ st) = "S:"   ++ show st
showTk (TkId         _ st) = "ID:"  ++ show st
showTk (TkTypeId     _ st) = "TID:" ++ show st
showTk (TkVariable _ st _) = "VR:"  ++ show st
showTk (TkFunction _ st _) = "FN:"  ++ show st
showTk (TkTypeVar  _ st  ) = "TV:"  ++ show st
showTk (TkLambda        _) = "LMDA"
showTk (TkExpr          _) = "EXPR"
showTk (TkNewline       _) = "NEWL"
showTk (TkBuiltin    _ st) = "BI:"  ++ show st
instance Show Token where show = showTk










showTokens :: [Token] -> String
showTokens tk = unwords $ map showTk tk


















