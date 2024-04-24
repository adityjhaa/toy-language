
all : clean
	@mkdir _build
	@ocamlc -c interpreter.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c parser.ml
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -c main.ml
	@ocamlfind ocamlc -linkpkg -package unix -o prolog interpreter.cmo parser.cmo lexer.cmo main.cmo
	@mv lexer.ml parser.ml parser.mli *.c* _build/
	@echo "Built complete!"

clean:
	@rm -rf _build/
	@rm -f prolog
	@echo "Cleaned your computer!"