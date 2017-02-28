TARGET := \
  main.native

PWD := \
  $(shell pwd)

ALPHALIB := \
  `ocamlfind query alphaLib`

SRC_DIR = src

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \
  -plugin-tag 'package(cppo_ocamlbuild)' \
  -tag "cppo_I($(ALPHALIB))" \
  -tag "cppo_I($(PWD))"

.PHONY: all test clean

all:
	@ $(OCAMLBUILD) $(SRC_DIR)/$(TARGET)

test: all
	@ ./$(TARGET)

include $(shell ocamlfind query visitors)/Makefile.preprocess

MLI := \
  $(patsubst %.ml,%.inferred.mli,ls src/*.ml | grep -v cppo | grep -v myocamlbuild)) \
  $(patsubst %.cppo.ml,%.inferred.mli,$(shell ls src/*.cppo.ml)) \

mli:
	@ $(OCAMLBUILD) src/grammar.inferred.mli

processed:
	make all || true
	make -C _build/src -f $(shell ocamlfind query visitors)/Makefile.preprocess grammar.processed.ml

clean:
	@ rm -f *~
	@ $(OCAMLBUILD) -clean
