.PHONY: all clean

all: main test

main: utils.ml model.ml main.ml
	ocamlfind ocamlopt -o $@ $^

test: utils.ml model.ml test.ml
	ocamlfind ocamlopt -o $@ $^

clean:
	rm -f *.cmi *.cmx *.o main test
