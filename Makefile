APP_SRC_DIR=src
APP_BIN_DIR=ebin
HDR_DIR=include
DOC_DIR=doc
DOC_PATTERN=index.html overview-summary.html modules-frame.html packages-frame.html stylesheet.css erlang.png edoc-info
TST_SRC_DIR=tests
TST_BIN_DIR=tests
EXM_DIR=examples
TMP_PATTERN=*~ \\\#*\\\# *.dump

APP_MODULES=proper proper_types proper_gen proper_symb proper_shrink proper_arith proper_extra
HEADERS=proper proper_internal proper_common
TST_MODULES=proper_tests
EXM_MODULES=mm
TXT_FILES=COPYING Makefile README $(DOC_DIR)/overview.edoc
RELEASE_FILE=proper.tar.gz

APP_SRC_FILES=$(addprefix $(APP_SRC_DIR)/, $(addsuffix .erl, $(APP_MODULES)))
APP_BIN_FILES=$(addprefix $(APP_BIN_DIR)/, $(addsuffix .beam, $(APP_MODULES)))
HDR_FILES=$(addprefix $(HDR_DIR)/, $(addsuffix .hrl, $(HEADERS)))
DOC_FILES=$(addprefix $(DOC_DIR)/, $(DOC_PATTERN) $(addsuffix .html, $(APP_MODULES)))
TST_SRC_FILES=$(addprefix $(TST_SRC_DIR)/, $(addsuffix .erl, $(TST_MODULES)))
TST_BIN_FILES=$(addprefix $(TST_BIN_DIR)/, $(addsuffix .beam, $(TST_MODULES)))
EXM_FILES=$(addprefix $(EXM_DIR)/, $(addsuffix .erl, $(EXM_MODULES)))
TMP_FILES=$(TMP_PATTERN) $(addprefix $(APP_SRC_DIR)/, $(TMP_PATTERN)) $(addprefix $(HDR_DIR)/, $(TMP_PATTERN)) $(addprefix $(DOC_DIR)/, $(TMP_PATTERN)) $(addprefix $(TST_SRC_DIR)/, $(TMP_PATTERN)) $(addprefix $(EXM_DIR)/, $(TMP_PATTERN))

ENTER_ERL=erl -noinput -eval '
EXIT_ERL=, halt().'
ERLC=erlc
ERLC_FLAGS=-W2 -Ddebug -DTEST +debug_info +warn_missing_spec +warn_untyped_record +inline -I $(HDR_DIR)
EDOC_OPTIONS=[{dialyzer_specs,all}, {report_missing_type,true}, {report_type_mismatch,true}, {pretty_print,erl_pp}, {preprocess,true}]
DIALYZER=dialyzer
DIALYZER_FLAGS=-Wunmatched_returns
NEEDED_APPS=compiler erts kernel stdlib crypto
RM=rm -f
TAR=tar -czf


# TODO: separate debug and optimization options
# TODO: extra targets: test, tags, commit/update
# TODO: header and text files as dependencies: more fine-grained

.PHONY: default all compile tests doc check clean distclean rebuild release build_plt

default: compile

all: compile tests doc

compile: $(APP_BIN_FILES)

$(APP_BIN_FILES): $(HDR_FILES)

$(APP_BIN_DIR)/%.beam: $(APP_SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(APP_BIN_DIR) $<

tests: $(TST_BIN_FILES)

$(TST_BIN_FILES): $(HDR_FILES)

$(TST_BIN_DIR)/%.beam: $(TST_SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(TST_BIN_DIR) $<

doc: $(APP_SRC_FILES) $(HDR_FILES) $(TXT_FILES)
	$(ENTER_ERL) edoc:application(proper, ".", $(EDOC_OPTIONS)) $(EXIT_ERL)

check: compile
	$(DIALYZER) $(DIALYZER_FLAGS) $(APP_BIN_FILES)

clean:
	$(RM) $(TMP_FILES)

distclean: clean
	$(RM) $(APP_BIN_FILES) $(DOC_FILES) $(TST_BIN_FILES) $(RELEASE_FILE)

rebuild: distclean compile

release: all clean
	$(RM) $(RELEASE_FILE)
	$(TAR) $(RELEASE_FILE) $(APP_SRC_FILES) $(APP_BIN_FILES) $(HDR_FILES) $(DOC_FILES) $(TST_SRC_FILES) $(EXM_FILES) $(TXT_FILES)

build_plt:
	$(DIALYZER) --build_plt --apps $(NEEDED_APPS)
