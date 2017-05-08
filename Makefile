TARGET := \
  main.native

EXECUTABLE := \
  rml

PWD := \
  $(shell pwd)

ALPHALIB := \
  `ocamlfind query alphaLib`

SRC_DIR = src

OCAMLBUILD := \
  ocamlbuild \
  -use-ocamlfind \
  -classic-display \
  -I $(SRC_DIR)/grammar \
  -I $(SRC_DIR)/print \
  -I $(SRC_DIR)/typing \
  -I $(SRC_DIR)/utils \
  -plugin-tag 'package(cppo_ocamlbuild)' \
  -tag "cppo_I($(ALPHALIB))" \
  -tag "cppo_I($(PWD))"

# Replace all files ending with .cppo.ml by .inferred.mli which is the
# extension of generated interfaces by ocamlbuild.
MLI := \
  $(patsubst %.cppo.ml,%.inferred.mli,$(shell ls $(SRC_DIR)/grammar/*.cppo.ml)) \

.PHONY: all test clean

default: all
test: all
	./rml -f test/MISC/comment.rml -a typing --use-stdlib
	./rml -f test/MISC/expression.rml -a typing --use-stdlib
	./rml -f test/MISC/field_selection.rml -a typing --use-stdlib
	./rml -f test/MISC/function.rml -a typing --use-stdlib
	./rml -f test/MISC/let_binding.rml -a typing --use-stdlib
	./rml -f test/MISC/local_binding.rml -a typing --use-stdlib
	./rml -f test/MISC/modules_functors.rml -a typing --use-stdlib
	./rml -f test/MISC/record.rml -a typing --use-stdlib
	./rml -f test/MISC/syntactic_sugar_multiple_param.rml -a typing --use-stdlib
	./rml -f test/subtype/stdlib.rml -a subtype --use-stdlib
	./rml -f test/subtype/simple_labels.rml -a subtype --use-stdlib
	./rml -f test/typing/list.rml -a typing --use-stdlib
	./rml -f test/typing/option.rml -a typing --use-stdlib
	./rml -f test/typing/sum.rml -a typing --use-stdlib
	./rml -f test/typing/pair_with.rml -a typing --use-stdlib

all:
	@ $(OCAMLBUILD) $(SRC_DIR)/$(TARGET)
	@ rm $(TARGET)
	@ rm -f $(PWD)/$(EXECUTABLE)
	@ ln -s _build/$(SRC_DIR)/$(TARGET) $(PWD)/$(EXECUTABLE)

mli:
	@ $(OCAMLBUILD) $(MLI)

clean:
	@ rm -f *~
	@ $(OCAMLBUILD) -clean
