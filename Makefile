SOURCES = programme_fichier.ml extraire.ml Etot.ml main.ml
TARGET = main

OCAMLC   = ocamlc -g
OCAMLOPT = ocamlopt -unsafe -noassert -inline 100
OCAMLDEP = ocamldep

OBJS = $(SOURCES:.ml=.cmo)
OBJS_OPT = $(SOURCES:.ml=.cmx)

all: .depend byte opt
byte: $(TARGET)
opt: $(TARGET).opt

$(TARGET): $(OBJS)
	$(OCAMLC) -o $@ $^

$(TARGET).opt: $(OBJS_OPT)
	$(OCAMLOPT) -o $@  $^

%.cmi: %.mli
	$(OCAMLC) $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

.PHONY: clean

clean:
	rm -f *.cm[iox] *~ *.o

cleanall: clean
	$(TARGET) $(TARGET).opt

.depend: $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend