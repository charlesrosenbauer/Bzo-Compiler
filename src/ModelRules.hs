module ModelRules where
import BzoTypes
import HigherOrder
import Data.Either
import Data.Maybe
import Debug.Trace











{-
wrappedModellerMap :: [BzoFileModel BzoSyntax] -> Either [BzoErr] [BzoFileModel BzoSyntax]
wrappedModellerMap ss =
  let xs = map (bfm_fileModel) ss
      er = concat $ lefts  xs
      vs = rights xs
      rets = map adjustAST $ zip ss $ map (\xs -> CA_Calls (ca_pos $ head xs) xs) vs
  in case er of
      []  -> Right rets
      ers -> Left ers
  where adjustAST :: Show a => (BzoFileModel a, BzoSyntax) -> BzoFileModel BzoSyntax
        adjustAST ((BzoFileModel mn fp dm _ fi fl fia fla), ast) = (BzoFileModel mn fp dm ast fi fl fia fla)
-}
