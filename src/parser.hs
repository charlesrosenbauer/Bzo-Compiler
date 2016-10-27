module BzoParser where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO hiding (try)
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
spaces = skipMany1 space










parseString :: Parser Token
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 many spaces
                 return $ TkStr x










parseBuiltin :: Parser Token
parseBuiltin = do char '$'
                  x <- many (letter <|> digit <|> symbol)
                  many spaces
                  return $ TkBuiltin x










parseTypeAtom :: Parser Token
parseTypeAtom = do first <- uppercase
                   rest  <- many (letter <|> digit <|> symbol)
                   many spaces
                   return $ TkTypeId ([first] ++ rest)










parseAtom :: Parser Token
parseAtom = do first <- lowercase <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               many spaces
               return $ TkId ([first] ++ rest)










parseInteger :: Parser Token
parseInteger = do num <- many1 digit
                  many spaces
                  return $ (TkInt . read) num










parseFloat :: Parser Token
parseFloat = do beg <- many1 digit
                char '.'
                end <- many1 digit
                many spaces
                return $ (TkFlt . read) (beg ++ "." ++ end)










parseSpecialGroup :: Parser Token
parseSpecialGroup = do x <- specialGroup
                       many spaces
                       return $ case x of
                           "()" -> TkTupEmpt
                           "[]" -> TkArrGnrl
                           "{}" -> TkExpGnrl
                           ".." -> TkArrMod
                           "::" -> TkDefine
                           ";;" -> TkFnSym
                           










parseSpecial :: Parser Token
parseSpecial = do x <- special
                  many spaces
                  return $ case x of
                    '(' -> TkStartTup
                    ')' -> TkEndTup
                    '[' -> TkStartDat
                    ']' -> TkEndDat
                    '{' -> TkStartDo
                    '}' -> TkEndDo
                    '.' -> TkSepExpr
                    ',' -> TkSepPoly
                    ':' -> TkFilterSym
                    ';' -> TkLambdaSym
                    '~' -> TkMutable
                    '@' -> TkReference
                    '_' -> TkWildcard
                    '\n'-> TkNewline










parseUnit :: Parser Token
parseUnit = parseTypeAtom
        <|> parseAtom
        <|> parseString
        <|> parseBuiltin
        <|> parseFloat
        <|> parseInteger
        <|> (try parseSpecialGroup)
        <|> (try parseSpecial)










parseExpr :: Parser [Token]
parseExpr = many parseUnit










showTk :: Token -> String
showTk TkStartTup      = "("
showTk TkEndTup        = ")"
showTk TkStartDat      = "["
showTk TkEndDat        = "]"
showTk TkSepExpr       = "."
showTk TkSepPoly       = ","
showTk TkFilterSym     = ":"
showTk TkLambdaSym     = ";"
showTk TkMutable       = "~"
showTk TkReference     = "@"
showTk TkWildcard      = "_"
showTk TkDefine        = "::"
showTk TkFnSym         = ";;"
showTk TkTupEmpt       = ")"
showTk TkArrGnrl       = "[]"
showTk TkExpGnrl       = "{}"
showTk TkArrMod        = ".."
showTk (TkInt x)       = "I:" ++ show x
showTk (TkFlt x)       = "F:" ++ show x
showTk (TkStr st)      = "S:" ++ st
showTk (TkId st)       = "ID:" ++ st
showTk (TkTypeId st)   = "TID:" ++ st
showTk (TkVariable st) = "VR:" ++ st
showTk (TkFunction st) = "FN:" ++ st
showTk (TkTypeVar st)  = "TV:" ++ st
showTk TkLambda        = "LMDA"
showTk TkExpr          = "EXPR"
showTk TkNewline       = "NEWL"
showTk (TkBuiltin st)  = "BI:" ++ st
instance Show Token where show = showTk










showTokens :: [Token] -> String
showTokens tk = unwords $ map showTk tk


















