structure Translate =
struct

fun compile (Ast.Const x)         = [Machine.Push x]
  | compile (Ast.Op (x, oper, y)) = List.concat [compile y , compile x , [Machine.Exec oper]];

end
