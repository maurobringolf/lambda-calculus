data List = Cons Int List | Nil

length l = case l of
  Cons i tl -> 1 + length tl;
  Nil -> 0;

main = length (Cons 2 (Cons 1 Nil))
