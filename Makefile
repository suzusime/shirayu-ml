# Usage:
# Type 'make bc' for byte code.
# Type 'make nc' for native code.
# Type 'make dc' for code with debugging information.
# For more detail, see http://mmottl.github.io/ocaml-makefile/

SOURCES=mySet.mli mySet.ml syntax.ml parser.mly lexer.mll environment.mli environment.ml eval.ml typing.ml main.ml
RESULT=miniml
YFLAGS=-v
OCAMLYACC=menhir

all: nc

test: $(RESULT)
	ocaml mySet.cmo syntax.cmo environment.cmo typing.cmo test.ml
	./typetest.sh

include OCamlMakefile
