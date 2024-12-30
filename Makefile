all: tp_script

tp_script: lexer.ml parser.ml ast.ml semantics.ml main.ml
	ocamlc -o tp_script ast.ml semantics.ml parser.mli parser.ml lexer.ml main.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

clean:
	rm -f tp_script parser.ml parser.mli lexer.ml *.cmi *.cmo
