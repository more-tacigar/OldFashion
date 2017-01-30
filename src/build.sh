rm *.cmi *.cmo ofparser.ml oflexer.ml

ocamlc -c ofsyntax.ml
ocamllex oflexer.mll
menhir ofparser.mly --infer
ocamlc -c ofparser.mli
ocamlc -c oflexer.ml
ocamlc -c ofparser.ml
ocamlc -c ofmain.ml
ocamlc -o oldfashion.native oflexer.cmo ofparser.cmo ofsyntax.cmo  ofmain.cmo
