
maxList :: (Ord a) => [a] -> a
maxList [] = error "empty list"
maxList [x] = x
maxList (x:xs) =
    let tempMax = [a | a <- xs, a >= x]
    in  maxList tempMax


zippingList :: (a -> b -> c) -> [a] -> [b] -> [c]
zippingList _ [] _ = []
zippingList _ _ [] = []
zippingList f (x:xs) (y:ys) = f x y : zippingList f xs ys