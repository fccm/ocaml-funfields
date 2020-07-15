OCAML = ocaml
#LIB_DIR = ../src
#LIB_DIR = +funfields
LIB_DIR = $(shell ocamlfind query funfields)
LIB_INC = -I $(LIB_DIR)

.PHONY: test
test:
	$(OCAML) $(LIB_INC) bits.cma test.ml

