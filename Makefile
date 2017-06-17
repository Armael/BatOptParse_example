all:
	ocamlbuild -use-ocamlfind head.native

clean:
	ocamlbuild -clean
