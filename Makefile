.PHONY: all

all:
	ocamlbuild -use-ocamlfind -pkg str myprogram.native
