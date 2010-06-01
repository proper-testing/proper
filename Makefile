MODULES=proper proper_types proper_gen proper_shrink proper_arith proper_tests proper_extra
HEADERS=proper proper_internal

SRC_DIR=src
HDR_DIR=include
BIN_DIR=ebin
DOC_DIR=doc
TMP_PATTERN=*~ \\\#*\\\# *.dump core

SRC_FILES=$(addprefix $(SRC_DIR)/, $(addsuffix .erl, $(MODULES)))
HDR_FILES=$(addprefix $(HDR_DIR)/, $(addsuffix .hrl, $(HEADERS)))
BIN_FILES=$(addprefix $(BIN_DIR)/, $(addsuffix .beam, $(MODULES)))
DOC_FILES=$(DOC_DIR)/*
TXT_FILES=COPYING Makefile README
RELEASE_FILE=proper.tar.gz
TMP_FILES=$(TMP_PATTERN) $(addprefix $(SRC_DIR)/, $(TMP_PATTERN)) $(addprefix $(HDR_DIR)/, $(TMP_PATTERN))

ERL=erl -noinput -eval
ERLC=erlc
ERLC_FLAGS=-W2 -Ddebug -DTEST +debug_info +warn_missing_spec +warn_untyped_record
EDOC_OPTIONS=[{overview, "$(SRC_DIR)/overview.edoc"}, {dialyzer_specs, all}, {report_missing_type, true}, {report_type_mismatch, true}, {pretty_print,erl_pp}, {preprocess, true}]
DIALYZER_OPTIONS=-Wunmatched_returns
RM=rm -f
TAR=tar -czf


# TODO: separate debug and optimization options
# TODO: rules for help/text files
# TODO: extra targets: test, tags, commit/update

.PHONY: default all compile doc check clean distclean rebuild release

default: compile

all: compile doc

compile: $(BIN_FILES)

# TODO: this should be more fine-grained
$(BIN_FILES): $(HDR_FILES)

$(BIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) -I $(HDR_DIR) -o $(BIN_DIR) $<

doc: $(SRC_FILES) $(HDR_FILES)
	$(ERL) 'edoc:application(proper, ".", $(EDOC_OPTIONS)), halt().'

check: compile
	dialyzer $(DIALYZER_OPTIONS) $(BIN_DIR)

clean:
	$(RM) $(TMP_FILES)

distclean: clean
	$(RM) $(BIN_FILES) $(DOC_FILES) $(RELEASE_FILE)

rebuild: distclean compile

release: all clean
	$(TAR) $(RELEASE_FILE) $(SRC_FILES) $(HDR_FILES) $(BIN_FILES) $(DOC_FILES) $(TXT_FILES)
