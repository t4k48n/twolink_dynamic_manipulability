.PHONY: all run clean

all: main test

main: utils.ml model.ml main.ml
	ocamlfind ocamlopt -package unix -linkpkg -o $@ $^

test: utils.ml model.ml test.ml
	ocamlfind ocamlopt -o $@ $^

run: main
	./main

clean:
	rm -f *.cmi *.cmx *.o main test
