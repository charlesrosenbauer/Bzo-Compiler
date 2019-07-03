module TypeClass where
import BzoTypes
import TypeChecker
import Query
import HigherOrder
import Data.Text
import Data.Int
import Data.List as L
import Data.Set  as S
import Data.Map.Strict as M
import Data.Maybe as Mb
import Debug.Trace









{-
checkTyClass :: DefinitionTable -> (TypeHeader, Type, TCId) -> [BzoErr]
checkTyClass dt@(DefinitionTable defs files ids _) (thead, ty, tc) =
  let
      -- Get typeclass definition and associated interface
      tclass :: Definition
      tclass = defs M.! tc

      intfc  :: [(Text, TypeHeader, Type)]
      intfc = interface tclass

      intfctable :: M.Map Text (TypeHeader, Type)
      intfctable = M.fromList $ L.map (\(a,b,c) -> (a, (b,c))) intfc


      -- Get local visibility
      visibility :: S.Set Int64
      visibility = S.fromList $ snd $ bfm_fileModel $ L.head $
                   L.filter (\x -> (pack $ bfm_filepath x) == (fileName $ typos ty)) $ files

      -- Line up interfaces
      fns :: [Text]
      fns = L.map fst3 intfc

      fns':: [(Text, [Int64])]
      fns'= L.map (\f -> (f, L.filter (\v -> S.member v visibility) $ Mb.fromMaybe [] $ M.lookup f ids)) fns


      -- Filter out functions that do not match interface
      -- NOTE: Make this keep track of TCs visited to avoid recursion.
      fitsInterface :: TCId -> Set TCId -> (TypeHeader, Type) -> Definition -> [BzoErr]
      fitsInterface tc tcset (th0, t0) fd@(FuncDef i _ th1 t1 _) =
        let
            errs :: [BzoErr]
            tcs  :: [(TypeHeader, Type, TCId)]
            (errs, _, tcs) = checkWithVars dt (th0, t0) (th1, t1)

            tcids :: Set TCId
            tcids = S.fromList $ L.map trd3 tcs
        in if (S.null tcids)
            then []
            else if (S.intersection tcset tcids /= S.empty)
                  then [TypeErr (typos t1) $ pack $ "Type Class " ++ (unpack $ identifier $ defs M.! tc) ++ " recurses with function " ++ (unpack i) ++ "."]
                  else fitsInterface tc (S.union tcset tcids) (th0, t0) fd

      -- This case shouldn't actually happen, but I'm including it just in case.
      -- Needs work to make it a bit more reliable though.
      fitsInterface _ _ _ _ = [TypeErr (BzoPos 0 0 $ pack "") $ pack "Expected a typeclass, found something else. This case shouldn't happen."]


      fnvals :: [[(Text, Int64)]]
      fnvals = []

  in if (L.null fnvals) || (L.any L.null fnvals)
      then [TypeErr (typos ty) $ pack $ "Type " ++ (show ty) ++ " does not match class " ++ (show tc) ++ "\n"]
      else []
-}
