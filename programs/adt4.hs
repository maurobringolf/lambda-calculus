data List = Node Int List | Nil

lift l = case l of
  Node i tl -> i:lift tl;
  Nil -> [];

main = lift (Node 8 (Node 1 (Node 2 Nil)))
