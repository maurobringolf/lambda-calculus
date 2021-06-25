main = map (\x -> x * 2) [1,2,3]

length = \xs -> xs (\x -> \a -> a + 1) 0

head = \xs -> xs (\a -> \b -> a) x

tail = \xs -> \c -> \n -> xs (\h -> \t -> \g -> g h (t c)) (\t -> n) (\h -> \t -> t)

map = \f -> \xs -> if length xs == 0 then [] else f (head xs) : map f (tail xs)

