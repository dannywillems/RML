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

all:
	@ $(OCAMLBUILD) $(SRC_DIR)/$(TARGET)
	@ rm $(TARGET)
	@ rm -f $(PWD)/$(EXECUTABLE)
	@ ln -s _build/$(SRC_DIR)/$(TARGET) $(PWD)/$(EXECUTABLE)

test: all
	@ ./$(TARGET)

mli:
	@ $(OCAMLBUILD) $(MLI)

clean:
	@ rm -f *~
	@ $(OCAMLBUILD) -clean
