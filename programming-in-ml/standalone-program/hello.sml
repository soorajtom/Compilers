(*

This is where the main function is defined. Use the helper main.sml
for compilation with mlton.

*)


structure Hello =
struct

fun main (name,args) = let val _ = print "Hello world\n"
	                   val _ = print ("program name: " ^ name ^ "\nArguments:\n")
			   val _ = List.map (fn x => print ("    " ^ x ^ "\n")) args
		       in 0 end;

end
