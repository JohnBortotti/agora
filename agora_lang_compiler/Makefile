all: main

lexer.ml: lexer.mll
	ocamllex lexer.mll

ast.cmi: ast.ml
	ocamlfind ocamlc -c ast.ml

code_gen.cmi: code_gen.ml ast.cmi
	ocamlfind ocamlc -c -package digestif.ocaml code_gen.ml

parser.ml parser.mli: parser.mly ast.cmi
	menhir --infer parser.mly

main: ast.cmo parser.cmo code_gen.cmo lexer.cmo main.cmo
	ocamlfind ocamlc -o main -linkpkg -package digestif.ocaml ast.cmo parser.cmo code_gen.cmo lexer.cmo main.cmo

ast.cmo: ast.ml
	ocamlfind ocamlc -c ast.ml

code_gen.cmo: code_gen.ml code_gen.cmi
	ocamlfind ocamlc -c -package digestif.ocaml code_gen.ml

parser.cmo: parser.ml
	ocamlfind ocamlc -c parser.mli
	ocamlfind ocamlc -c parser.ml

lexer.cmo: lexer.ml
	ocamlfind ocamlc -c lexer.ml

main.cmo: main.ml
	ocamlfind ocamlc -c main.ml

clean:
	rm -f *.cmo *.cmi main lexer.ml parser.ml parser.mli
