module BzoExprModeller where
import BzoTypes
import BzoEmulator





modelFunctions :: DefinitionTable -> Either [BzoErr] FnTable
modelFunctions (DefinitionTable defs fls ids _) =
  let
      {-
        --Model functions--

        TODO:
          * Convert function expressions to Interpreter Expressions
          * Handle Typeclasses
          * Handle Overloading
          * Handle Scopes
          * Handle Namespaces
      -}

  in Left []
