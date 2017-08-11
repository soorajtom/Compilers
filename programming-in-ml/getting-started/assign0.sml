datatype 'a Tree = Null
                 | Node of 'a Tree * 'a * 'a Tree; 


fun inorder Null = []
  | inorder (Node(a, b, c)) = inorder a @ [b] @ inorder c;
  
  
