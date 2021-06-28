length = foldr (\x a -> a + 1) 0

head = foldr (\a b -> a) undefined

tail xs c n = foldr (\h t g -> g h (t c)) (\t -> n) xs (\h t -> t)

map f xs = foldr (\x ys -> f x : ys) [] xs
