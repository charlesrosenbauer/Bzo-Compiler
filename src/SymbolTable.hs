module SymbolTable where
import BzoTypes
import qualified Data.Text  as T
import qualified Data.Map   as M
import qualified Data.Maybe as Mb
import qualified Data.List  as L










addFile :: SymbolTable -> T.Text -> SymbolTable
addFile st@(SymbolTable iids fids itab ftab itop ftop) file =
  case (M.lookup file fids) of
    Nothing -> (SymbolTable iids (M.insert file (ftop+1) fids) itab (M.insert (ftop+1) file ftab) itop (ftop+1))
    Just _  -> st










addSymbol :: T.Text -> SymbolTable -> T.Text -> SymbolTable
addSymbol file st@(SymbolTable iids fids itab ftab itop ftop) symbol =
  let st'@(SymbolTable iids' fids' itab' ftab' itop' ftop') =
        case (M.lookup file fids) of
          Nothing -> addFile st file
          Just _  -> st
      fid = Mb.fromMaybe (-1) $ M.lookup file   fids' -- Should never return -1
      iid = Mb.fromMaybe []   $ M.lookup symbol iids'
  in case iid of
      [] -> -- Symbol has no previous usage
        (SymbolTable (M.insert symbol [(fid, itop'+1)] iids') fids' (M.insert (itop'+1) (symbol, fid) itab') ftab' (itop'+1) ftop')
      xs -> -- Symbol has been used before
        case (L.lookup fid xs) of
          Just _  -> st' -- Symbol has been used in the current file. No need for additions
          Nothing ->
            (SymbolTable (M.insert symbol ((fid, itop'+1):xs) iids') fids' (M.insert (itop'+1) (symbol, fid) itab') ftab' (itop'+1) ftop')
