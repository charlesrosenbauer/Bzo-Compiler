module HigherOrder where
import qualified Data.List as L hiding (foldl, map)
import qualified Data.Either as E
import qualified Data.Maybe as M
import qualified Data.Map.Strict as Mp hiding (foldl, map)







fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a










snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b










trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c










app_1_2 :: (a -> c) -> (a, b) -> (c, b)
app_1_2 f (a, b) = (f a, b)










app_2_2 :: (b -> c) -> (a, b) -> (a, c)
app_2_2 f (a, b) = (a, f b)










app_1_3 :: (a -> d) -> (a, b, c) -> (d, b, c)
app_1_3 f (a, b, c) = (f a, b, c)










app_2_3 :: (b -> d) -> (a, b, c) -> (a, d, c)
app_2_3 f (a, b, c) = (a, f b, c)










app_3_3 :: (c -> d) -> (a, b, c) -> (a, b, d)
app_3_3 f (a, b, c) = (a, b, f c)










app_3_12 :: (a -> d) -> (b -> e) -> (a, b, c) -> (d, e, c)
app_3_12 f g (a, b, c) = (f a, g b, c)










app_3_13 :: (a -> d) -> (c -> e) -> (a, b, c) -> (d, b, e)
app_3_13 f g (a, b, c) = (f a, b, g c)










app_3_23 :: (b -> d) -> (c -> e) -> (a, b, c) -> (a, d, e)
app_3_23 f g (a, b, c) = (a, f b, g c)










-- An If/Else can sometimes be inconvenient
ife :: Bool -> a -> a -> a
ife True  x y = x
ife False x y = y










makeIntMap :: Int -> [k] -> (Mp.Map Int k, Int)
makeIntMap i ks =
  let xs   = zip (map (+i) [0..]) ks
      reti = i + length xs
  in (insertMany Mp.empty xs, reti)










doubleInsertMany :: Ord k0 => Ord k1 => [(k0, k1, a)] -> Mp.Map k0 (Mp.Map k1 a) -> M.Maybe (Mp.Map k0 (Mp.Map k1 a))
doubleInsertMany []     m = Just m
doubleInsertMany (x:xs) m =
  case (doubleInsert x m) of
    Just m' -> doubleInsertMany xs m'
    Nothing -> Nothing










doubleSearch :: Ord k0 => Ord k1 => (k0, k1) -> Mp.Map k0 (Mp.Map k1 a) -> M.Maybe a
doubleSearch (k0, k1) m =
  case (Mp.lookup k0 m) of
    Nothing -> Nothing
    Just a  -> Mp.lookup k1 a










doubleInsert :: Ord k0 => Ord k1 => (k0, k1, a) -> Mp.Map k0 (Mp.Map k1 a) -> M.Maybe (Mp.Map k0 (Mp.Map k1 a))
doubleInsert (k0, k1, x) m =
  case (Mp.lookup k0 m) of
    Nothing -> Just $ Mp.insert k0 (Mp.insert k1 x Mp.empty) m
    Just a  ->
      case (Mp.lookup k1 a) of
        Just b  -> Nothing
        Nothing -> Just $ Mp.adjust (Mp.insert k1 x) k0 m









repeatUntilFilterStops :: (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
repeatUntilFilterStops filt xform xs =
  let xs' = L.filter filt $ xform xs
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
  let (ls, rs) = E.partitionEithers xs
  in  if((length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ rs










concatMapWithErrs' :: Either a [b] -> (b -> Either a [c]) -> Either a [c]
concatMapWithErrs' xs fn =
  case xs of
    Left  err -> Left err
    Right val ->
      let (ls, rs) = E.partitionEithers $ map fn val
      in  if((length ls) /= 0)
        then Left  $ ls !! 0
        else Right $ concat rs










concatMapWithErrs :: [a] -> (a -> E.Either b [c]) -> E.Either b [c]
concatMapWithErrs xs fn =
  let (ls, rs) = E.partitionEithers $ map fn xs
  in  if((length ls) /= 0)
    then Left  $ ls !! 0
    else Right $ concat rs










tryMaybeList :: a -> [a -> M.Maybe b] -> M.Maybe b
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










maybeMerge :: [a] -> M.Maybe [a] -> [a]
maybeMerge a (Just b) = a ++ b
maybeMerge a _        = a










maybeIf :: Bool -> a -> M.Maybe a
maybeIf True  x = Just x
maybeIf False x = Nothing










insertMany :: Ord k => Mp.Map k a -> [(k, a)] -> Mp.Map k a
insertMany m xs = L.foldl' (\mp (k, a) -> Mp.insert k a mp) m xs










insertManyList :: Ord k => Mp.Map k [a] -> [(k, a)] -> Mp.Map k [a]
insertManyList m xs = L.foldl' (\mp (k, a) ->
  if (Mp.member k mp)
    then Mp.adjust (\as -> as ++ [a]) k mp
    else Mp.insert k [a] mp
  ) m xs










fromListToListMap :: Ord k => [(k, [a])] -> Mp.Map k [a]
fromListToListMap xs = L.foldl' (\mp (k, a) ->
  if (Mp.member k mp)
    then Mp.adjust (\as -> as ++ a) k mp
    else Mp.insert k a mp
  ) (Mp.empty) xs









applyWithErr :: (b -> E.Either a c) -> E.Either a b -> E.Either a c
applyWithErr f x =
  case x of
    Left  a -> Left  a
    Right b -> f b










applyWithErrM :: Monad m => (b -> m (E.Either a c)) -> E.Either a b -> m (E.Either a c)
applyWithErrM f x =
  case x of
    Left  a -> return $ Left  a
    Right b -> f b










applyWithErrList :: (b -> E.Either a c) -> E.Either [a] b -> E.Either [a] c
applyWithErrList f x =
  case x of
    Left  a -> Left a
    Right b ->
      case f b of
        Left  a -> Left [a]
        Right b -> Right b










applyRight :: (b -> c) -> E.Either a b -> Either a c
applyRight f (Left  a) = Left  a
applyRight f (Right b) = Right $ f b










applyLeft :: (a -> c) -> E.Either a b -> Either c b
applyLeft f (Left  a) = Left  $ f a
applyLeft f (Right b) = Right b










containsManyMembers :: Ord a => Mp.Map a b -> [a] -> Bool
containsManyMembers mp as = all (\x -> Mp.member x mp) as











zip2Map :: [([a], [b])] -> ([a], [b])
zip2Map = ((\(x, y) -> (concat x, concat y)) . unzip)










zip3Map :: [([a], [b], [c])] -> ([a], [b], [c])
zip3Map = ((\(x, y, z) -> (concat x, concat y, concat z)) . unzip3)










shrinkPairs :: (Eq a, Ord a) => [(a, [b])] -> [(a, [b])]
shrinkPairs xs =
  let ks  = map (\(a, b) -> (a, [])) xs
      mp  = insertMany Mp.empty ks
      mp' = L.foldl' (\m (k, a) -> Mp.adjust (\x -> x ++ a) k m) mp xs
  in Mp.assocs mp'










shrinkPairs3 :: (Eq a, Ord a) => [(a, [b], [c], [d])] -> [(a, [b], [c], [d])]
shrinkPairs3 xs =
  let ks  = map (\(a, b, c, d) -> (a, ([], [], []))) xs
      mp  = insertMany Mp.empty ks
      mp' = L.foldl' (\m (k, a, b, c) -> Mp.adjust (\(x, y, z) -> (x ++ a, y ++ b, z ++ c)) k m) mp xs
  in map (\(a, (b, c, d)) -> (a, b, c, d)) $ Mp.assocs mp'
