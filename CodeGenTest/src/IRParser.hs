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
  | PI_Ptr      {ppos :: IRPos, num0 :: Int}
  | PI_Arr      {ppos :: IRPos, num0 :: Int}
  | PI_MultiArr {ppos :: IRPos, nums :: [Int]}
  | PI_Type     {ppos :: IRPos, nums :: [Int], num0 :: Int, txt0 :: Text}
  | PI_Node     {ppos :: IRPos, num0 :: Int, txt0 :: Text, pars :: [IRParseItem]}
  | PI_NS       {ppos :: IRPos, nums :: [Int]}
  | PI_Nodes    {ppos :: IRPos, pars :: [IRParseItem]}
  | PI_Def      {ppos :: IRPos, par0 :: IRParseItem}
  | PI_Defs     {ppos :: IRPos, pars :: [IRParseItem]}
  | PI_Const    {ppos :: IRPos, txt0 :: Text}
  | PI_ConstInt {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_ConstStr {ppos :: IRPos, txt0 :: Text, txt1 :: Text}
  | PI_HintInt  {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_HintStr  {ppos :: IRPos, txt0 :: Text, txt1 :: Text}
  | PI_Str      {ppos :: IRPos, txt0 :: Text}
  | PI_Int      {ppos :: IRPos, num0 :: Int}
  | PI_Func     {ppos :: IRPos, txt0 :: Text}
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



-- Pointers -- *
irParseIter fname tokens ((PI_Token _ (PtrToken p1))
                   :(PI_Token _ (PtrToken p0))        : stk)  = irParseIter fname tokens ((PI_Ptr      p0  2      ) : stk)

irParseIter fname tokens ((PI_Token _ (PtrToken p1   ))
                   :(PI_Ptr    p0 n )                 : stk)  = irParseIter fname tokens ((PI_Ptr      p0  (n + 1)) : stk)

irParseIter fname tokens ((PI_Ptr    p0 n)            : stk)  = irParseIter fname tokens ((PI_Ptr      p0  1      ) : stk)



-- Newlines -- \n
irParseIter fname tokens ((PI_Token _ (NewLine  p1))
                   :(PI_NL     p0  )                  : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

irParseIter fname tokens ((PI_Token _ (NewLine  p1))
                   :(PI_Token _ (NewLine  p0))        : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

irParseIter fname ((PI_Token _ (NewLine p1)):tokens) (
                    (PI_Token _ (NewLine  p0))        : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)

irParseIter fname tokens ((PI_Token _ (NewLine  p0))  : stk)  = irParseIter fname tokens ((PI_NL       p0)          : stk)



-- Arrays -- [N]
irParseIter fname tokens ((PI_Token _ (ArrToken p1 n1))
                   :(PI_Token _ (ArrToken p0 n0)):      stk)  = irParseIter fname tokens ((PI_MultiArr p0 [n1, n0]) : stk)

irParseIter fname tokens ((PI_Token _ (ArrToken p1 n0))
                   :(PI_MultiArr p0 ns)        :        stk)  = irParseIter fname tokens ((PI_MultiArr p0 (n0: ns)) : stk)

irParseIter fname tokens ((PI_Token _ (ArrToken p0 n0)):stk)  = irParseIter fname tokens ((PI_MultiArr p0  [n0]   ) : stk)



-- Nodes -- #N op #A #B ... \n
--irParseIter fname tokens ((PI_NL    p3              )
--                   :params
--                   :(PI_Token _ (FuncToken p1 fn))
--                   :(PI_Token _ (NumToken  p0 n0)):     stk) = irParseIter fname tokens ((PI_Node p0 n0 fn params) : stk)



-- Types -- Ty | *Ty | [n]Ty | ...
irParseIter fname tokens ((PI_Token _ (TypeToken p2 ty))
                   :(PI_Ptr      p1 pn         )
                   :(PI_MultiArr p0 as         )      : stk) = irParseIter fname tokens ((PI_Type p0 as pn ty)     : stk)

irParseIter fname tokens ((PI_Token _ (TypeToken p1 ty))
                   :(PI_Ptr   p0 pn            )      : stk) = irParseIter fname tokens ((PI_Type p0 [] pn ty)     : stk)

irParseIter fname tokens ((PI_Token _ (TypeToken p1 ty))
                   :(PI_MultiArr p0 as         )      : stk) = irParseIter fname tokens ((PI_Type p0 as 0 ty)      : stk)

irParseIter fname tokens ((PI_Token _ (TypeToken p0 ty)):stk) = irParseIter fname tokens ((PI_Type p0 [] 0  ty)     : stk)



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
                   :(PI_Type  p1 [] 0 tyid)
                   :(PI_Token _ (DefType   p0))       : stk) = irParseIter fname tokens ((PI_TyHeader p0 tyid n)       :stk)

irParseIter fname tokens ((PI_NL p4)
                   :(PI_Token _ (OpenBrace p3))
                   :(PI_NS    p2 ns)
                   :(PI_Type  p1 [] 0 tyid)
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
                   :(PI_Token _ (TypeToken p4 ty))
                   :(PI_NS    p2 [n0, n1])
                   :(PI_Token _ (ProcToken p1 prid))
                   :(PI_Token _ (DefProc   p0))       : stk) = irParseIter fname tokens ((PI_PrHeader p0 prid n0 n1 ty):stk)

irParseIter fname tokens ((PI_NL p6)
                   :(PI_Token _ (OpenBrace p5))
                   :(PI_Token _ (TypeToken p4 ty))
                   :(PI_NS    p2 ns)
                   :(PI_Token _ (ProcToken p1 prid))
                   :(PI_Token _ (DefProc   p0))       : stk) = Left $ IRErr p1 $ pack ("Expected 2 numerical parameter in type header. Found " ++ (show $ L.length ns) ++ ".")



-- Procedure Definition
irParseIter fname tokens ((PI_NL p3)
                   :(PI_Token _ (CloseBrace p2))
                   :(PI_Nodes p1 ns)
                   :(PI_PrHeader p0 prid n0 n1 ty)    : stk) = irParseIter fname tokens ((PI_PrDef p0 prid n0 n1 ty ns):stk)

irParseIter fname tokens ((PI_NL p2)
                   :(PI_Token _ (CloseBrace p1))
                   :(PI_PrHeader p0 prid n0 n1 ty)    : stk) = Left $ IRErr p1 $ pack "Expected contents for the procedure declaration."



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

irParseIter fname tokens (t@(PI_Type p1 _ _ _)
                   :(PI_Node p0 num fnid pars)        : stk) = irParseIter fname tokens ((PI_Node p0 num fnid (t:pars))               :stk)

irParseIter fname tokens ((PI_NL p1)
                   :node@(PI_Node p0 _ _ _)           : stk) = irParseIter fname tokens ((PI_Nodes p0 [node])                         :stk)

irParseIter fname tokens ((PI_Nodes p1 nodes1)
                   :(PI_Nodes p0 nodes0)              : stk) = irParseIter fname tokens ((PI_Nodes p0 (nodes0 ++ nodes1))             :stk)



-- No rules work?
irParseIter fname [] [item] = Right item

irParseIter fname [] (s:stack) = Left $ IRErr (ppos s) $ pack ("Parser could not consume entire file.\nStack:\n" ++ (L.concatMap (\s -> show s ++ "\n") (s:stack)))

irParseIter fname (t:tokens) stack = irParseIter fname tokens (t:stack)











irParse :: String -> [IRToken] -> Either IRErr IRParseItem
irParse fname tks = irParseIter fname (L.map (\t -> PI_Token (tpos t) t) tks) []
