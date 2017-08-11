datatype 'a Tree = Null
                 | Node of 'a Tree * 'a * 'a Tree; 


fun inorder Null = []
  | inorder (Node(a, b, c)) = inorder a @ [b] @ inorder c;
  
  
fun anticlockwise Null = Null
  | anticlockwise (Node(a, b, Node(c1, c2, c3))) = Node(Node(a, b, c1), c2, c3)
  | anticlockwise (Node(a, b, Null)) = Node(a, b, Null);
