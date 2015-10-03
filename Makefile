.PHONY: clean test

src := test.ml

test: $(src) filter.native
	ocamlfind ocamlc -dsource -ppx ./filter.native -linkpkg -package batteries -o test test.ml
	rm *.cm*

lambda: $(src)
	ocaml -dlambda $(src)

typedtree: $(src)
	ocaml -dtypedtree $(src)

parsetree: $(src)
	ocaml -dparsetree $(src)

filter.native: filter.ml
	ocamlbuild -package compiler-libs.common -package batteries filter.native
	
clean:
	ocamlbuild -clean
