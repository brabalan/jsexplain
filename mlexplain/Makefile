all: mlexplain

GENERATOR_DIR := ../generator

OCAMLDEP := ocamldep -one-line
OCAMLPAR := OCAMLRUNPARAM="l=200M"

LINEOF_BIN := $(GENERATOR_DIR)/lineof.byte
LINEOF     := $(OCAMLPAR) $(LINEOF_BIN)

PPX_BIN    := $(GENERATOR_DIR)/monad_ppx.byte
PPX_FLAG   := -ppx $(PPX_BIN)

MLTOJS_BIN := $(GENERATOR_DIR)/main.byte
MLTOJS 	   := $(OCAMLPAR) $(MLTOJS_BIN) $(PPX_FLAG)
# -dsource is automatically considered by main.byte

ASSEMBLY_BIN := $(GENERATOR_DIR)/assembly.byte
ASSEMBLY     := $(ASSEMBLY_BIN)

DISPLAYGEN_BIN := $(GENERATOR_DIR)/displayed_sources.byte
DISPLAYGEN     := $(OCAMLPAR) $(DISPLAYGEN_BIN)

# Warn if buildtools are missing
$(MLTOJS_BIN) $(LINEOF_BIN) $(PPX_BIN) $(ASSEMBLY_BIN) $(DISPLAYGEN_BIN):
	$(error Missing generator tools, build from project root, or set paths.)

STDLIB_DIR := $(GENERATOR_DIR)/stdlib_ml
STDLIB_FLAG := -I $(STDLIB_DIR) -open Stdlib
REPLACE_STDLIB := -nopervasives -nostdlib $(STDLIB_FLAG)

ASSEMBLY_JS_FILES := \
	MLSyntax.unlog.js \
	Map.unlog.js \
	MLInterpreter.log.js

ASSEMBLY_JS := \
	$(STDLIB_DIR)/stdlib.js \
	$(ASSEMBLY_JS_FILES)

DISPLAYED := \
	./MLInterpreter.ml

ALL_DISPLAYED := $(DISPLAYED:.ml=.unlog.js) $(DISPLAYED:.ml=.pseudo.js) $(DISPLAYED)
ALL_LINEOF := $(DISPLAYED:.ml=.token.js) $(DISPLAYED:.ml=.mlloc.js) $(DISPLAYED:.ml=.ptoken.js)

%.cmi: %.ml $(MLTOJS_BIN)
	$(MLTOJS) -mode cmi -I $(STDLIB_DIR) $<

%.log.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode log -I $(STDLIB_DIR) $<

%.unlog.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode unlog -I $(STDLIB_DIR) $<

%.token.js %.mlloc.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode token -I $(STDLIB_DIR) $<

%.pseudo.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode pseudo -I $(STDLIB_DIR) $<

%.ptoken.js: %.ml %.cmi $(MLTOJS_BIN)
	$(MLTOJS) -mode ptoken -I $(STDLIB_DIR) $<

assembly.js: $(ASSEMBLY_JS) $(ASSEMBLY_BIN)
	$(ASSEMBLY) -o $@ $(ASSEMBLY_JS)

displayed_sources.js: $(ALL_DISPLAYED) $(DISPLAYGEN_BIN)
	$(DISPLAYGEN) -o $@ $(ALL_DISPLAYED)

lineof.js: $(ALL_LINEOF) $(LINEOF_BIN)
	$(LINEOF) -o $@ $(ALL_LINEOF)

# mlexplain.byte: MLInterpreter.cmo
	# ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.ppx -linkpkg -o $@ $^

# mlexplain.js: 
	# js_of_ocaml $<

mlexplain: assembly.js lineof.js displayed_sources.js 

clean:
	rm *.cmo || true
	rm *.cmi || true
	rm *.cmx || true
	rm mlexplain.byte || true
	rm *.js || true
