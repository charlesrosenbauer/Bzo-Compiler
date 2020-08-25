{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2020 Charles Rosenbauer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.-}

module BzoParser where
import Data.Text as T
import Data.List as L
import BzoTypes
import BzoParserRules
import AST
import Tokens
import Error
import HigherOrder










isBracket :: BzoToken -> Bool
isBracket (TkStartDo  _) = True
isBracket (TkStartTup _) = True
isBracket (TkStartDat _) = True
isBracket (TkEndDo    _) = True
isBracket (TkEndTup   _) = True
isBracket (TkEndDat   _) = True
isBracket _              = False










-- Probably could use a monad, but that's too much work
combineBracketChecks :: (Text -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> (Text -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> Text -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
combineBracketChecks f0 f1 fname tks =
  let a = case (f0 fname tks) of
            (Just errs, tks') -> (errs, f1 fname tks')
            (Nothing  , tks') -> ([]  , f1 fname tks')
  in case a of
      ([],      (Nothing   , tks')) -> (Nothing              , tks')
      (errs0,   (Just errs1, tks')) -> (Just (errs0 ++ errs1), tks')
      (errs0,   (Nothing   , tks')) -> (Just errs0           , tks')











recoverBracketCheck :: (Text -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> Text -> [BzoErr] -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
recoverBracketCheck fn fname errs tks =
  let out = fn fname tks
  in case out of
    (Nothing   , tks') -> (Just errs           , tks')
    (Just errs0, tks') -> (Just (errs ++ errs0), tks')










bracketCheck_Tuple :: Text -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Tuple fname ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Tuple fname tks
bracketCheck_Tuple fname ((TkEndTup   ps) : tks) = (Nothing, tks)
bracketCheck_Tuple fname ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data  bracketCheck_Tuple fname tks
bracketCheck_Tuple fname ((TkEndDat   ps) : tks) = recoverBracketCheck bracketCheck_Tuple fname [ParseErr ps $ pack "Invalid placement of ']' inside Tuple"] tks
bracketCheck_Tuple fname ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Tuple fname tks
bracketCheck_Tuple fname ((TkEndDo    ps) : tks) = recoverBracketCheck bracketCheck_Tuple fname [ParseErr ps $ pack "Invalid placement of '}' inside Tuple"] tks
bracketCheck_Tuple fname ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 fname) $ pack "Mismatched parentheses"], [])
bracketCheck_Tuple fname (tk : tks)              = (Just $ [ParseErr (spos tk) $ pack "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Tuple function"], tks)










bracketCheck_Data :: Text -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Data fname ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Data fname tks
bracketCheck_Data fname ((TkEndTup   ps) : tks) = recoverBracketCheck bracketCheck_Data fname [ParseErr ps $ pack "Invalid placement of ')' inside Array Modifier"] tks
bracketCheck_Data fname ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data bracketCheck_Data  fname tks
bracketCheck_Data fname ((TkEndDat   ps) : tks) = (Nothing, tks)
bracketCheck_Data fname ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Data fname tks
bracketCheck_Data fname ((TkEndDo    ps) : tks) = recoverBracketCheck bracketCheck_Data fname [ParseErr ps $ pack "Invalid placement of '}' inside Array Modifier"] tks
bracketCheck_Data fname ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 fname) $ pack "Mismatched Square Brackets"], [])
bracketCheck_Data fname (tk : tks)              = (Just $ [ParseErr (spos tk) $ pack "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Data function"], tks)










bracketCheck_Block :: Text -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Block fname ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Block fname tks
bracketCheck_Block fname ((TkEndTup   ps) : tks) = recoverBracketCheck bracketCheck_Block fname [ParseErr ps $ pack "Invalid placement of ')' inside Block"] tks
bracketCheck_Block fname ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data bracketCheck_Block  fname tks
bracketCheck_Block fname ((TkEndDat   ps) : tks) = recoverBracketCheck bracketCheck_Block fname [ParseErr ps $ pack "Invalid placement of ']' inside Block"] tks
bracketCheck_Block fname ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Block fname tks
bracketCheck_Block fname ((TkEndDo    ps) : tks) = (Nothing, tks)
bracketCheck_Block fname ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 fname) $ pack "Mismatched Braces"], [])
bracketCheck_Block fname (tk : tks)              = (Just $ [ParseErr (spos tk) $ pack "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Block function"], tks)









bracketCheckFn :: Text -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheckFn fname (t : ts) = case t of
          (TkStartTup ps) -> bracketCheck_Tuple fname ts
          (TkStartDat ps) -> bracketCheck_Data  fname ts
          (TkStartDo  ps) -> bracketCheck_Block fname ts
          (TkEndTup   ps) -> (Just $ [ParseErr ps $ pack "Mismatched parentheses!"], ts)
          (TkEndDat   ps) -> (Just $ [ParseErr ps $ pack "Mismatched square brackets!"], ts)
          (TkEndDo    ps) -> (Just $ [ParseErr ps $ pack "Mismatched braces!"], ts)
          tk              -> (Just $ [ParseErr (spos tk) $ pack "This error should not occur. Please notify the developer that something is wrong in the bracketCheckFn function"], ts)










bracketCheck :: Text -> [BzoToken] -> Maybe [BzoErr]
bracketCheck fname tks =
  let tks' = L.filter isBracket tks
  in checkIter tks' (bracketCheckFn fname)
    where checkIter x f = case x of
            [] -> Nothing
            _  -> case (f x) of
                    (Nothing, []) -> Nothing
                    (Nothing, ts) -> checkIter ts f
                    (Just es, []) -> Just es
                    (Just es, ts) -> Just $ maybeMerge es (checkIter ts f)










fuseNameObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseNameObj (BzS_TyId p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExTypObj p0 i0 i1)]
fuseNameObj (BzS_Id   p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExFunObj p0 i0 i1)]
fuseNameObj a                b                     = [a, b]










parseFile :: Text -> [BzoToken] -> Either [BzoErr] BzoSyntax
parseFile fname tks =
  let bracketErrs = bracketCheck fname tks
      parseOut    = parserIter fname (L.map (\t -> BzS_Token (spos t) t) tks) []
  in case (bracketErrs, parseOut) of
      (Just errs,        _ ) -> Left errs
      (Nothing  , Left errs) -> Left errs
      (Nothing  , Right ast) -> Right ast
