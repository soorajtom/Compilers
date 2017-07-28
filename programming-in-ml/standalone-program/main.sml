(*

This is a stand alone ml program. We recommend the use of the mlton
compiler to compile such standalone programs. However, for responsive
development and quick prototyping mlton is not a good option as it
does not have a interpreter (REPL).

*)


val name = CommandLine.name();
val args = CommandLine.arguments();
val _    = Hello.main (name , args);
