main = length [7,8,1]

length = \xs -> foldr (\x -> \a -> a + 1) 0 xs
