.PHONY: all

all:
	ocamlbuild -use-ocamlfind -tag safe_string -pkg str myprogram.native
