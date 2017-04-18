.PHONY: all

all:
	ocamlbuild -use-ocamlfind -tag safe_string -pkg tyre -pkg containers myprogram.native
