module IRParser where
import IRLexer
import Data.List as L
import Data.Text
import Debug.Trace










data IRParseItem
  = PI_FnHeader {ppos :: IRPos, txt0 :: Text, num0 :: Int, num1 :: Int}
  | PI_FnDef    {ppos :: IRPos, txt0 :: Text, num0 :: Int, num1 :: Int, pars :: [IRParseItem]}
  | PI_TyHeader {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_TyDef    {ppos :: IRPos, txt0 :: Text, num0 :: Int, pars :: [IRParseItem]}
  | PI_PrHeader {ppos :: IRPos, txt0 :: Text, num0 :: Int, num1 :: Int, txt1 :: Text}
  | PI_PrDef    {ppos :: IRPos, txt0 :: Text, num0 :: Int, num1 :: Int, txt2 :: Text, pars :: [IRParseItem]}
  | PI_ExHeader {ppos :: IRPos, txt0 :: Text, num0 :: Int, num1 :: Int}
  | PI_ExDef    {ppos :: IRPos, txt0 :: Text, num0 :: Int, num1 :: Int, pars :: [IRParseItem]}
  | PI_NL       {ppos :: IRPos}
  -- | PI_Ptr      {ppos :: IRPos, num0 :: Int}
  | PI_Arr      {ppos :: IRPos, num0 :: Int}
  -- | PI_MultiArr {ppos :: IRPos, nums :: [Int]}
  | PI_Type     {ppos :: IRPos, nums :: [Int], txt0 :: Text}
  | PI_Node     {ppos :: IRPos, num0 :: Int, txt0 :: Text, pars :: [IRParseItem]}
  | PI_NS       {ppos :: IRPos, nums :: [Int]}
  | PI_Nodes    {ppos :: IRPos, pars :: [IRParseItem]}
  | PI_Defs     {ppos :: IRPos, pars :: [IRParseItem]}
  | PI_Const    {ppos :: IRPos, txt0 :: Text}
  | PI_ConstInt {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_ConstStr {ppos :: IRPos, txt0 :: Text, txt1 :: Text}
  | PI_HintInt  {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_HintStr  {ppos :: IRPos, txt0 :: Text, txt1 :: Text}
  | PI_Str      {ppos :: IRPos, txt0 :: Text}
  | PI_Int      {ppos :: IRPos, num0 :: Int}
  | PI_Func     {ppos :: IRPos, txt0 :: Text}
  | PI_Proc     {ppos :: IRPos, txt0 :: Text}
  | PI_Extn     {ppos :: IRPos, txt0 :: Text}
  | PI_Token    {ppos :: IRPos, tkn :: IRToken}
  deriving (Eq, Show)










irParseIter :: String -> [IRParseItem] ->  [IRParseItem] -> Either IRErr IRParseItem
irParseIter fname [] [] = Left $ IRErr (IRPos 1 1 $ pack fname) $ pack "Nothing to parse?"



-- Numbers -- #N
irParseIter fname tokens ((PI_Token _ (NumToken p1 n1))
                   :(PI_Token _ (NumToken p0 n0))     : stk)  = irParseIter fname tokens ((PI_NS       p0 [n1, n0]) : stk)

irParseIter fname tokens ((PI_Token _ (NumToken p1 n0))
                   :(PI_NS     p0 ns)                 : stk)  = irParseIter fname tokens ((PI_NS       p0 (n0: ns)) : stk)

irParseIter fname tokens ((PI_Token _ (NumToken p0 n0)):stk)  = irParseIter fname tokens ((PI_NS       p0  [n0]   ) : stk)



-- Newlines -- \n
irParseIter fname tokens ((PI_Token _ (NewLine  p1))
                   :(PI_NL     p0  )                  : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

irParseIter fname tokens ((PI_Token _ (NewLine  p1))
                   :(PI_Token _ (NewLine  p0))        : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

irParseIter fname ((PI_Token _ (NewLine p1)):tokens) (
                    (PI_Token _ (NewLine  p0))        : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

irParseIter fname tokens ((PI_Token _ (NewLine  p0))  : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)



-- Types -- Ty | *Ty | [n]Ty | ...
-- Pointers are represented by an array of size 0
irParseIter fname tokens ((PI_Type p1 ns ty)
                   :(PI_Token p0 (PtrToken _  ))      : stk) = irParseIter fname tokens ((PI_Type p0 (0:ns)  ty)    : stk)

irParseIter fname tokens ((PI_Type p1 ns ty)
                   :(PI_Token p0 (ArrToken _ as))     : stk) = irParseIter fname tokens ((PI_Type p0 (as:ns) ty)    : stk)

irParseIter fname tokens ((PI_Token _ (TypeToken p0 ty)):stk)= irParseIter fname tokens ((PI_Type p0 [] ty)         : stk)




-- Fn Header
irParseIter fname tokens ((PI_NL p4)
                   :(PI_Token _ (OpenBrace p3))
                   :(PI_NS    p2 [n0, n1])
                   :(PI_Token _ (FuncToken p1 fnid))
                   :(PI_Token _ (DefFunc   p0))       : stk) = irParseIter fname tokens ((PI_FnHeader p0 fnid n0 n1)   :stk)

irParseIter fname tokens ((PI_NL p4)
                   :(PI_Token _ (OpenBrace p3))
                   :(PI_NS    p2 ns)
                   :(PI_Token _ (FuncToken p1 fnid))
                   :(PI_Token _ (DefFunc   p0))       : stk) = Left $ IRErr p1 $ pack ("Expected 2 numerical parameters in function header. Found " ++ (show $ L.length ns) ++ ".")



-- Function Definition
irParseIter fname tokens ((PI_NL p3)
                   :(PI_Token _ (CloseBrace p2))
                   :(PI_Nodes p1 ns)
                   :(PI_FnHeader p0 fnid n0 n1)       : stk) = irParseIter fname tokens ((PI_FnDef p0 fnid n0 n1 ns)   :stk)

irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token _ (CloseBrace p1))
                   :(PI_FnHeader p0 fnid n0 n1)       : stk) = Left $ IRErr p1 $ pack "Expected contents for the function declaration."



-- Ty Header
irParseIter fname tokens ((PI_NL p4)
                   :(PI_Token _ (OpenBrace p3))
                   :(PI_NS    p2 [n])
                   :(PI_Type  p1 [] tyid)
                   :(PI_Token _ (DefType   p0))       : stk) = irParseIter fname tokens ((PI_TyHeader p0 tyid n)       :stk)

irParseIter fname tokens ((PI_NL p4)
                   :(PI_Token _ (OpenBrace p3))
                   :(PI_NS    p2 ns)
                   :(PI_Type  p1 [] tyid)
                   :(PI_Token _ (DefType   p0))       : stk) = Left $ IRErr p1 $ pack ("Expected 1 numerical parameter in type header. Found " ++ (show $ L.length ns) ++ ".")


-- Type Definition
irParseIter fname tokens ((PI_NL p3)
                   :(PI_Token _ (CloseBrace p2))
                   :(PI_Nodes p1 ns)
                   :(PI_TyHeader p0 tyid n)           : stk) = irParseIter fname tokens ((PI_TyDef p0 tyid n ns)       :stk)

irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token _ (CloseBrace p1))
                   :(PI_TyHeader p0 tyid n)           : stk) = Left $ IRErr p1 $ pack "Expected contents for the type declaration."



-- Pr Header
irParseIter fname tokens ((PI_NL p6)
                   :(PI_Token _ (OpenBrace p5))
                   :(PI_Type  p3 [] ty)
                   :(PI_NS    p2 [n0, n1])
                   :(PI_Token _ (ProcToken p1 prid))
                   :(PI_Token _ (DefProc   p0))       : stk) = irParseIter fname tokens ((PI_PrHeader p0 prid n0 n1 ty):stk)

irParseIter fname tokens ((PI_NL p6)
                   :(PI_Token _ (OpenBrace p5))
                   :(PI_Token _ (TypeToken p4 ty))
                   :(PI_NS    p2 ns)
                   :(PI_Token _ (ProcToken p1 prid))
                   :(PI_Token _ (DefProc   p0))       : stk) = Left $ IRErr p1 $ pack ("Expected 2 numerical parameters in procedure header. Found " ++ (show $ L.length ns) ++ ".")



-- Procedure Definition
irParseIter fname tokens ((PI_NL p3)
                   :(PI_Token _ (CloseBrace p2))
                   :(PI_Nodes p1 ns)
                   :(PI_PrHeader p0 prid n0 n1 ty)    : stk) = irParseIter fname tokens ((PI_PrDef p0 prid n0 n1 ty ns):stk)

irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token _ (CloseBrace p1))
                   :(PI_PrHeader p0 prid n0 n1 ty)    : stk) = Left $ IRErr p1 $ pack "Expected contents for the procedure declaration."



-- Ex Header
irParseIter fname tokens ((PI_NL p4)
                   :(PI_Token _ (OpenBrace p3))
                   :(PI_NS    p2 [n0, n1])
                   :(PI_Token _ (ExternToken p1 exid))
                   :(PI_Token _ (DefExtern p0))       : stk) = irParseIter fname tokens ((PI_ExHeader p0 exid n0 n1)   :stk)

irParseIter fname tokens ((PI_NL p4)
                   :(PI_Token _ (OpenBrace p3))
                   :(PI_NS    p2 ns)
                   :(PI_Token _ (ProcToken p1 prid))
                   :(PI_Token _ (DefExtern p0))       : stk) = Left $ IRErr p1 $ pack ("Expected 2 numerical parameters in extern header. Found " ++ (show $ L.length ns) ++ ".")


-- Extern Definition
irParseIter fname tokens ((PI_NL p3)
                   :(PI_Token _ (CloseBrace p2))
                   :(PI_Nodes p1 ns)
                   :(PI_ExHeader p0 exid n0 n1)       : stk) = irParseIter fname tokens ((PI_ExDef p0 exid n0 n1 ns)   :stk)

irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token _ (CloseBrace p1))
                   :(PI_ExHeader p0 exid n0 n1)       : stk) = Left $ IRErr p1 $ pack "Expected contents for the procedure declaration."



-- Constant Definitions
irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token p1 (StrToken _ str))
                   :(PI_Token p0 (ConstToken _ cid))  : stk) = irParseIter fname tokens ((PI_ConstStr p0 cid str)      :stk)

irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token p1 (NumToken _ num))
                   :(PI_Token p0 (ConstToken _ cid))  : stk) = irParseIter fname tokens ((PI_ConstInt p0 cid num)      :stk)



-- Hint Definitions
irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token p1 (StrToken _ str))
                   :(PI_Token p0 (HintToken _ hid))   : stk) = irParseIter fname tokens ((PI_HintStr p0 hid str)       :stk)

irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token p1 (NumToken _ num))
                   :(PI_Token p0 (HintToken _ hid))   : stk) = irParseIter fname tokens ((PI_HintInt p0 hid num)       :stk)



-- Nodes
irParseIter fname tokens ((PI_Token p1 (FuncToken _ fnid))
                   :(PI_Token p0 (NodeToken _ num))   : stk) = irParseIter fname tokens ((PI_Node p0 num fnid [])      :stk)

irParseIter fname tokens ((PI_Token p1 (NodeToken _ n))
                   :(PI_Node p0 num fnid pars)        : stk) = irParseIter fname tokens ((PI_Node p0 num fnid ((PI_Int  p1 n):pars))  :stk)

irParseIter fname tokens ((PI_Token p1 (FuncToken _ f))
                   :(PI_Node p0 num fnid pars)        : stk) = irParseIter fname tokens ((PI_Node p0 num fnid ((PI_Func p1 f):pars))  :stk)

irParseIter fname tokens ((PI_Token p1 (ExternToken _ e))
                   :(PI_Node p0 num fnid pars)        : stk) = irParseIter fname tokens ((PI_Node p0 num fnid ((PI_Extn p1 e):pars))  :stk)

irParseIter fname tokens ((PI_Token p1 (ProcToken _ p))
                   :(PI_Node p0 num fnid pars)        : stk) = irParseIter fname tokens ((PI_Node p0 num fnid ((PI_Proc p1 p):pars))  :stk)

irParseIter fname tokens ((PI_Token p1 (StrToken _ s))
                   :(PI_Node p0 num fnid pars)        : stk) = irParseIter fname tokens ((PI_Node p0 num fnid ((PI_Str p1 s):pars))  :stk)

irParseIter fname tokens ((PI_Token p1 (NumToken _ n))
                   :(PI_Node p0 num fnid pars)        : stk) = irParseIter fname tokens ((PI_Node p0 num fnid ((PI_Int p1 n):pars))  :stk)

irParseIter fname tokens (t@(PI_Type p1 _ _)
                   :(PI_Node p0 num fnid pars)        : stk) = irParseIter fname tokens ((PI_Node p0 num fnid (t:pars))               :stk)

irParseIter fname tokens ((PI_NL p1)
                   :node@(PI_Node p0 _ _ _)           : stk) = irParseIter fname tokens ((PI_Nodes p0 [node])                         :stk)

irParseIter fname tokens ((PI_Nodes p1 nodes1)
                   :(PI_Nodes p0 nodes0)              : stk) = irParseIter fname tokens ((PI_Nodes p0 (nodes0 ++ nodes1))             :stk)



-- Wrap everything up with a nice little bow
irParseIter fname tokens (fndef@(PI_FnDef p0 _ _ _ _)   : stk) = irParseIter fname tokens ((PI_Defs p0 [fndef])                       :stk)

irParseIter fname tokens (tydef@(PI_TyDef p0 _ _ _  )   : stk) = irParseIter fname tokens ((PI_Defs p0 [tydef])                       :stk)

irParseIter fname tokens (prdef@(PI_PrDef p0 _ _ _ _ _) : stk) = irParseIter fname tokens ((PI_Defs p0 [prdef])                       :stk)

irParseIter fname tokens (exdef@(PI_ExDef p0 _ _ _ _)   : stk) = irParseIter fname tokens ((PI_Defs p0 [exdef])                       :stk)

irParseIter fname tokens (csdef@(PI_ConstInt p0 _ _ )   : stk) = irParseIter fname tokens ((PI_Defs p0 [csdef])                       :stk)

irParseIter fname tokens (csdef@(PI_ConstStr p0 _ _ )   : stk) = irParseIter fname tokens ((PI_Defs p0 [csdef])                       :stk)

irParseIter fname tokens (htdef@(PI_HintInt  p0 _ _ )   : stk) = irParseIter fname tokens ((PI_Defs p0 [htdef])                       :stk)

irParseIter fname tokens (htdef@(PI_HintStr  p0 _ _ )   : stk) = irParseIter fname tokens ((PI_Defs p0 [htdef])                       :stk)

irParseIter fname tokens ((PI_Defs p1 ds1)
                   :(PI_Defs p0 ds0)                    : stk) = irParseIter fname tokens ((PI_Defs p0 (ds0 ++ ds1))                  :stk)

irParseIter fname tokens ((PI_NL p1)
                   :(PI_Defs p0 ds0)                    : stk) = irParseIter fname tokens ((PI_Defs p0 ds0) : stk)



-- No rules work?
irParseIter fname [] [item] = Right item

irParseIter fname [] (s:stack) = Left $ IRErr (ppos s) $ pack ("Parser could not consume entire file.\nStack:\n" ++ (L.concatMap (\s -> show s ++ "\n") (s:stack)))

irParseIter fname (t:tokens) stack = irParseIter fname tokens (t:stack)











irParse :: String -> [IRToken] -> Either IRErr IRParseItem
irParse fname tks = irParseIter fname (L.map (\t -> PI_Token (tpos t) t) tks) []
