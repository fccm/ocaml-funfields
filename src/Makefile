OCAML = ocaml
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDOC = ocamldoc
OCAMLFIND = ocamlfind
DOCDIR = doc
MKDIR = mkdir -p

.PHONY: all lib opt
all: lib opt
lib: bits.cma
opt: bits.cmxa bits.cmxs

.SECONDARY:

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<

%.cma: %.cmo
	$(OCAMLC) -a -o $@ $<

%.cmxa: %.cmx
	$(OCAMLOPT) -a -o $@ $<

%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

.PHONY: findinstall
findinstall:
	$(OCAMLFIND) install funbits META *.cma *.cmx[as] *.cmi *.mli *.a

.PHONY: finduninstall
finduninstall:
	$(OCAMLFIND) remove funbits

.PHONY: doc
doc:
	$(MKDIR) $(DOCDIR)
	$(OCAMLDOC) -d $(DOCDIR) -html bits.mli

.PHONY: clean
clean:
	$(RM) doc/* *.cm[ioxat] *.cmx[as] *.[oa]

