data Tree = Node Int Tree Tree | Leaf


preorder t = case t of
  Leaf -> [];
  Node i t1 t2 -> concat (i:preorder t1) (preorder t2);

main = preorder (Node 11 Leaf (Node 2 Leaf (Node 7 Leaf Leaf)))
