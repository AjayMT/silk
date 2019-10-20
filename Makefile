
SOURCES = src/parsetree.ml src/lexer.mll src/parser.mly src/silk.ml
RESULT = silk
OCAMLYACC = menhir

include OCamlMakefile
