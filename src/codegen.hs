module CodeGen where
import HigherOrder
import BzoReducedAST










-- | Something to start with
data BzoFunction
  = BzF_Standard{  -- There are going to be more kinds of functions internally. Functions that have special memory interactions, etc.
      fnName :: String,
      fnId   :: Int
      -- fnIn :: [Variable]
      -- fnOut:: [Variable]
      -- fnVar:: [Variable]
      -- fnExp:: [Expr]
      -- Or maybe I should do something else. Not sure. Trying to come up with ideas.
    }
