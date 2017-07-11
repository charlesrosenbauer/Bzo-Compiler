module HigherOrder where
import Data.List hiding (foldl, map)
import Data.Either
import Data.Maybe
import Data.Map.Strict hiding (foldl, map)










repeatUntilFilterStops :: (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
repeatUntilFilterStops filt xform xs =
  let xs' = Data.List.filter filt $ xform xs
  in if ((length xs') == (length xs))
      then xs'
      else repeatUntilFilterStops filt xform xs'











applyIf :: (a -> a) -> a -> Bool -> a
applyIf f x True  = f x
applyIf f x False = x










applyIfE :: (a -> b) -> (a -> b) -> a -> Bool -> b
applyIfE f g x True  = f x
applyIfE f g x False = g x









data Either3 a b c = ChoiceA a | ChoiceB b | ChoiceC c deriving (Eq, Show)










aChoices :: [Either3 a b c] -> [a]
aChoices xs = concatMap fn xs
                where fn (ChoiceA x) = [x]
                      fn _           = [ ]










bChoices :: [Either3 a b c] -> [b]
bChoices xs = concatMap fn xs
                where fn (ChoiceB x) = [x]
                      fn _           = [ ]










cChoices :: [Either3 a b c] -> [c]
cChoices xs = concatMap fn xs
                where fn (ChoiceC x) = [x]
                      fn _           = [ ]










eitherFirst :: (a -> Either b c) -> (a -> Either b d) -> a -> Either3 b c d
eitherFirst f0 f1 x =
  let x0 = f0 x
      x1 = f1 x
  in case (x0, x1) of
      (Left  a, Left  b) -> ChoiceA a
      (Left  a, Right b) -> ChoiceC b
      (Right a, _      ) -> ChoiceB a










mapEither :: Either a b -> (b -> c) -> Either a c
mapEither e f =
  case e of
    Left  err -> Left err
    Right val -> Right $ f val










applyListFns :: [(b -> c)] -> ((b -> c) -> a -> d) -> [[a]] -> [[d]]
applyListFns fs fn xs = map (\(f, x) -> map (fn f) x) $ zip fs xs










sepErrs :: [Either a [b]] -> Either a [[b]]
sepErrs xs =
  let (ls, rs) = partitionEithers xs
  in  if((length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ rs










concatMapWithErrs' :: Either a [b] -> (b -> Either a [c]) -> Either a [c]
concatMapWithErrs' xs fn =
  case xs of
    Left  err -> Left err
    Right val ->
      let (ls, rs) = partitionEithers $ map fn val
      in  if((length ls) /= 0)
        then Left  $ ls !! 0
        else Right $ concat rs










concatMapWithErrs :: [a] -> (a -> Either b [c]) -> Either b [c]
concatMapWithErrs xs fn =
  let (ls, rs) = partitionEithers $ map fn xs
  in  if((length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ concat rs










tryMaybeList :: a -> [a -> Maybe b] -> Maybe b
tryMaybeList a []       = Nothing
tryMaybeList a (f : fs) =
  case (f a) of
    Just x  -> Just x
    Nothing -> tryMaybeList a fs










simplifyList :: [a] -> (a -> a -> [a]) -> [a]
simplifyList xs f = (\(a, b) -> a ++ b) $ simplifyList' ([], xs) f










simplifyList' :: ([a], [a]) -> (a -> a -> [a]) -> ([a], [a])
simplifyList' (l, [])         f = (l, [])
simplifyList' (l, [x])        f = (l ++ [x], [])
simplifyList' (l, [a, b])     f =
  case (f a b) of
    [a', b'] -> (l ++ [a'], [b'])
    [x']     -> (l        , [x'])
simplifyList' (l, (a: b: xs)) f =
  case (f a b) of
    [a', b'] -> simplifyList' (l ++ [a'], [b'] ++ xs) f
    [x']     -> simplifyList' (l        , [x'] ++ xs) f










maybeMerge :: [a] -> Maybe [a] -> [a]
maybeMerge a (Just b) = a ++ b
maybeMerge a _        = a










maybeIf :: Bool -> a -> Maybe a
maybeIf True  x = Just x
maybeIf False x = Nothing










insertMany :: Ord k => Map k a -> [(k, a)] -> Map k a
insertMany m xs = Data.List.foldl' (\mp (k, a) -> Data.Map.Strict.insert k a mp) m xs










insertManyList :: Ord k => Map k [a] -> [(k, a)] -> Map k [a]
insertManyList m xs = Data.List.foldl' (\mp (k, a) ->
  if (member k mp)
    then Data.Map.Strict.adjust (\as -> as ++ [a]) k mp
    else Data.Map.Strict.insert k [a] mp
  ) m xs









applyWithErr :: (b -> Either a c) -> Either a b -> Either a c
applyWithErr f x =
  case x of
    Left  a -> Left  a
    Right b -> f b










applyWithErrM :: Monad m => (b -> m (Either a c)) -> Either a b -> m (Either a c)
applyWithErrM f x =
  case x of
    Left  a -> return $ Left  a
    Right b -> f b










applyWithErrList :: (b -> Either a c) -> Either [a] b -> Either [a] c
applyWithErrList f x =
  case x of
    Left  a -> Left a
    Right b ->
      case f b of
        Left  a -> Left [a]
        Right b -> Right b










containsManyMembers :: Ord a => Map a b -> [a] -> Bool
containsManyMembers mp as = all (\x -> member x mp) as











zip2Map :: [([a], [b])] -> ([a], [b])
zip2Map = ((\(x, y) -> (concat x, concat y)) . unzip)










zip3Map :: [([a], [b], [c])] -> ([a], [b], [c])
zip3Map = ((\(x, y, z) -> (concat x, concat y, concat z)) . unzip3)










shrinkPairs :: (Eq a, Ord a) => [(a, [b])] -> [(a, [b])]
shrinkPairs xs =
  let ks  = map (\(a, b) -> (a, [])) xs
      mp  = insertMany empty ks
      mp' = Data.List.foldl' (\m (k, a) -> adjust (\x -> x ++ a) k m) mp xs
  in assocs mp'










shrinkPairs3 :: (Eq a, Ord a) => [(a, [b], [c], [d])] -> [(a, [b], [c], [d])]
shrinkPairs3 xs =
  let ks  = map (\(a, b, c, d) -> (a, ([], [], []))) xs
      mp  = insertMany empty ks
      mp' = Data.List.foldl' (\m (k, a, b, c) -> adjust (\(x, y, z) -> (x ++ a, y ++ b, z ++ c)) k m) mp xs
  in map (\(a, (b, c, d)) -> (a, b, c, d)) $ assocs mp'
