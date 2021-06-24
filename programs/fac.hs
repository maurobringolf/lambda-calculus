fac = \n -> if n == 0 then 1 else (n * (fac (n - 1)))

main = fac 2
