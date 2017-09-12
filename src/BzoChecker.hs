module BzoChecker where
import BzoTypes
import SymbolTable










data DefState
  = DefState {
      ds_calls   :: [CallAST],
      ds_defs    :: [Definition],
      ds_errs    :: [BzoErr],
      ds_current :: Definition }
