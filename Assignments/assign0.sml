datatype 'a Tree = Null
                 | Node of 'a Tree * 'a * 'a Tree; 


fun inorder Null = []
  | inorder (Node(a, b, c)) = inorder a @ [b] @ inorder c;
  
  
fun anticlockwise (Node(a, b, Node(c1, c2, c3))) = Node(Node(a, b, c1), c2, c3)
  | anticlockwise x = x;
  
fun head a = Node(Null, a, Null);

val mytree = Node( head 5, 6 ,Node(head 0,7,Null));
