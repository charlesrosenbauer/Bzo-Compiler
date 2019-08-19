module Core where
import Data.Int
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Set  as S
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import BzoTypes
import Error
import AST
import HigherOrder










data (Show a) => BzoFileModel a
  = BzoFileModel{
      bfm_moduleName    :: !T.Text,
      bfm_filepath      :: !FilePath,
      bfm_domain        :: !T.Text,
      bfm_fileModel     :: !a,
      bfm_fileImports   :: ![T.Text],
      bfm_fileLinks     :: ![T.Text],
      bfm_fileImportsAs :: ![(T.Text, T.Text)],   -- [(import fst, refer to as snd)]
      bfm_fileLinksAs   :: ![(T.Text, T.Text)]}   -- [(link   fst, refer to as snd)]










showBzoFileModel :: Show a => BzoFileModel a -> String
showBzoFileModel (BzoFileModel mn pth dmn ast imp lnk impa lnka) =
  "\n \nModule: " ++ (show mn ) ++ "\nPath: " ++ (show pth) ++
    "\nDomain: "  ++ (show dmn) ++
    "\nImports: " ++ (show imp) ++ "\nAliased Imports: " ++ (show impa) ++
    "\nLinks: "   ++ (show lnk) ++ "\nAliased Links: "   ++ (show lnka) ++
    "\nAST:\n"    ++ (show ast)
instance (Show a) => Show (BzoFileModel a) where show = showBzoFileModel










replaceModel :: (Show a, Show b) => BzoFileModel a -> b -> BzoFileModel b
replaceModel (BzoFileModel mn fp dm _ fi fl ia la) x = (BzoFileModel mn fp dm x fi fl ia la)










adjustModel :: (Show a, Show b) => BzoFileModel a -> (a -> b) -> BzoFileModel b
adjustModel (BzoFileModel mn fp dm x fi fl ia la) f = (BzoFileModel mn fp dm (f x) fi fl ia la)










data Definition
 = FuncDef {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typehead   :: !TypeHeader,
    functype   :: !Type,
    definitions:: ![(BzoSyntax)] }
 | TypeDef {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typehead   :: !TypeHeader,
    typedef    :: !Type }
 | TyClassDef {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typehead   :: !TypeHeader,
    interface  :: ![(T.Text, TypeHeader, Type)] }
 | ImplDef {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    classimpl  :: !T.Text,
    hostfile   :: !T.Text,
    impldefs   :: ![(T.Text, BzoSyntax)] }
 | FuncSyntax {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    ftyheader  :: !BzoSyntax,
    funcsyntax :: ![BzoSyntax] }
 | TypeSyntax {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typesyntax :: !BzoSyntax }
 | TyClassSyntax {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typesyntax :: !BzoSyntax }
 | ImplSyntax {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    classimpl  :: !T.Text,
    hostfile   :: !T.Text,
    implsyntax :: ![BzoSyntax] }
  deriving Eq


-- Not sure how I should handle Impl yet. Might not need to be handled at all.
dtdef :: Definition -> Int64 -> Type
dtdef (FuncDef  _ _ _ _ t _) _ = t
dtdef (TypeDef  _ _ _ _ t  ) _ = t
dtdef (TyClassDef p _ _ _ i) c = TyCsType p c i










showDefinition :: Definition -> String
showDefinition (FuncDef _ fnid file tyhd fty defs) = "  FNDEF:\n    " ++
                                              "FNID: " ++ (show fnid)  ++ "\n    " ++
                                              "FILE: " ++ (show file)  ++ "\n    " ++
                                              "TYHD: " ++ (show tyhd)  ++ "\n    " ++
                                              "TYPE: " ++ (show fty)   ++ "\n    " ++
                                              "DEFS: " ++ (show defs)  ++ "\n\n"

showDefinition (TypeDef _ tyid file thd defs) = "  TYDEF:\n    " ++
                                              "TYID: " ++ (show tyid)  ++ "\n    " ++
                                              "FILE: " ++ (show file)  ++ "\n    " ++
                                              "TYHD: " ++ (show thd)   ++ "\n    " ++
                                              "DEFS: " ++ (show defs)  ++ "\n\n"

showDefinition (TyClassDef _ tcid file thd ifac) = "  TCDEF:\n    " ++
                                              "TCID: " ++ (show tcid)  ++ "\n    " ++
                                              "FILE: " ++ (show file)  ++ "\n    " ++
                                              "TYHD: " ++ (show thd)   ++ "\n    " ++
                                              "FNCS: " ++ (show ifac)  ++ "\n\n"
showDefinition (ImplDef _ imid tcid file ifac) = "  IMPL:\n    " ++
                                              "IMPT: " ++ (show imid)  ++ "\n    " ++
                                              "TCID: " ++ (show tcid)  ++ "\n    " ++
                                              "FILE: " ++ (show file)  ++ "\n    " ++
                                              "DEFS: " ++ (show ifac)  ++ "\n\n"

showDefinition (FuncSyntax _ fnid file hedr defs) = "  FNSYN:\n    " ++
                                              (show fnid)  ++ "\n    " ++
                                              (show file)  ++ "\n    T:  " ++
                                              (show hedr)  ++ "\n    D:  " ++
                                              (show defs)  ++ "\n"

showDefinition (TypeSyntax _ tyid file defs) = "  TYSYN:\n    " ++
                                              (show tyid)  ++ "\n    " ++
                                              (show file)  ++ "\n    D:  " ++
                                              (show defs)

showDefinition (TyClassSyntax _ tyid file defs) = "  TCSYN:\n    " ++
                                              (show tyid)  ++ "\n    " ++
                                              (show file)  ++ "\n    D:  " ++
                                              (show defs)

showDefinition (ImplSyntax _ imid tcid file defs) = "  IMPL:\n    " ++
                                              (show imid)  ++ "\n   " ++
                                              (show tcid)  ++ "\n   " ++
                                              (show file)  ++ "\n   D:  " ++
                                              (show defs)

instance Show Definition where show = showDefinition










data DefinitionTable
  = DefinitionTable {
      dt_defs :: !(M.Map Int64 Definition),
      dt_files:: ![BzoFileModel ([Int64], [Int64])], -- [(local defs, visible defs)]
      dt_ids  :: !(M.Map T.Text [Int64]),
      dt_top  :: !Int64 }
  deriving Show
