natN n = n : natN (n+1)

nat = natN 0

main = take 3 nat
