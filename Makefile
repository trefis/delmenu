all: delmenu

delmenu: instance.ml main.ml
	ocamlfind ocamlopt -g -package core,efl,str -linkpkg -thread -o "$@" $^

clean:
	@rm *.cmi
	@rm *.cmx
	@rm *.o
	@rm delmenu

.PHONY: all clean
