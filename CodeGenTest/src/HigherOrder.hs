module HigherOrder where










{-
  NOTE:
  When this portion of the compiler gets merged back into the main compiler pipeline,
  this file needs to be merged with the main HigherOrder.hs file.
-}









-- Change later and just import HigherOrder.hs
ife :: Bool -> a -> a -> a
ife True  a b = a
ife False a b = b










appendPair :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
appendPair (x0, y0) (x1, y1) = (x0 ++ x1, y0 ++ y1)










appendEither :: Either a b -> ([a], [b]) -> ([a], [b])
appendEither (Left  x) (xs, ys) = (x:xs,   ys)
appendEither (Right y) (xs, ys) = (  xs, y:ys)










onLeft  :: (a -> c) -> Either a b -> Either c b
onLeft f (Left  l) = Left (f l)
onLeft f (Right r) = Right r










onRight :: (b -> c) -> Either a b -> Either a c
onRight f (Right r) = Right (f r)
onRight f (Left  l) = Left l










getRights :: Either a [b] -> [b]
getRights (Left  x) = []
getRights (Right r) = r










getLefts  :: Either [a] b -> [a]
getLefts  (Left  x) = x
getLefts  (Right r) = []










justRight :: Either a b -> b
justRight (Right x) = x










justLeft  :: Either a b -> a
justLeft  (Left  x) = x
