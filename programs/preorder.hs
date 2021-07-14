data Tree = Node Int Tree Tree | Leaf

concat xs ys = if length xs == 0 then
                 ys
               else
                 (head xs) : concat (tail xs) ys

preorder t = case t of
  Leaf -> [];
  Node i t1 t2 -> concat (i:preorder t1) (preorder t2);

main = preorder (Node 11 Leaf (Node 2 Leaf (Node 7 Leaf Leaf)))
