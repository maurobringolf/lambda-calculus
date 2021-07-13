length = foldr (\x a -> a + 1) 0

head = foldr (\a b -> a) undefined

map f xs = foldr (\x ys -> f x : ys) [] xs

take n xs = if n == 0 then [] else head xs : take (n - 1) (tail xs)

filter f = foldr (\x ys -> if f x then x:ys else ys) []
