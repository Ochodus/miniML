export PATH=/home/mario/.opam/4.13.1+mingw64c/bin:/usr/x86_64-w64-mingw32/sys-root/mingw/bin:/usr/local/bin:/usr/bin:/cygdrive/c/Program:
export OCAMLLIB=C:/OCaml64/home/mario/.opam/4.13.1+mingw64c/lib/ocaml

all: run

run: lexer.cmo parser.cmo lang.cmo interpreter_static.cmo interpreter_dynamic.cmo main.cmo
	ocamlc -o run lexer.cmo parser.cmo lang.cmo interpreter_static.cmo interpreter_dynamic.cmo main.cmo

lang.cmo : lang.ml
	ocamlc -c lang.ml

parser.ml: parser.mly lang.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

interpreter_static.cmo: interpreter_static.ml
	ocamlc -c interpreter_static.ml

interpreter_dynamic.cmo: interpreter_dynamic.ml
	ocamlc -c interpreter_dynamic.ml

main.cmo : lang.cmo interpreter_static.cmo interpreter_dynamic.cmo main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
