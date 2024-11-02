all: main

lexer.ml: lexer.mll
	ocamllex lexer.mll

ast.cmi: ast.ml
	ocamlc -c ast.ml

parser.ml parser.mli: parser.mly ast.cmi
	menhir --infer parser.mly

main: ast.cmo parser.cmo lexer.cmo main.cmo
	ocamlc -o main ast.cmo parser.cmo lexer.cmo main.cmo

ast.cmo: ast.ml
	ocamlc -c ast.ml

parser.cmo: parser.ml
	ocamlc -c parser.mli
	ocamlc -c parser.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

main.cmo: main.ml
	ocamlc -c main.ml

clean:
	rm -f *.cmo *.cmi main lexer.ml parser.ml parser.mli

