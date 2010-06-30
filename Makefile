APP_SRC_DIR=src
APP_BIN_DIR=ebin
HDR_DIR=include
DOC_DIR=doc
DOC_PATTERN=index.html overview-summary.html modules-frame.html packages-frame.html stylesheet.css erlang.png edoc-info
TST_SRC_DIR=tests
TST_BIN_DIR=tests
EXM_DIR=examples
TMP_PATTERN=*~ \\\#*\\\# *.dump

TXT_FILES=COPYING Makefile README $(DOC_DIR)/overview.edoc
RELEASE_FILE=proper.tar.gz

APP_SRC_FILES=$(wildcard $(APP_SRC_DIR)/*.erl)
APP_MODULES=$(APP_SRC_FILES:$(APP_SRC_DIR)/%.erl=%)
APP_BIN_FILES=$(APP_MODULES:%=$(APP_BIN_DIR)/%.beam)
HDR_FILES=$(wildcard $(HDR_DIR)/*.hrl)
DOC_FILES=$(addprefix $(DOC_DIR)/, $(DOC_PATTERN) $(addsuffix .html, $(APP_MODULES)))
TST_SRC_FILES=$(wildcard $(TST_SRC_DIR)/*.erl)
TST_MODULES=$(TST_SRC_FILES:$(TST_SRC_DIR)/%.erl=%)
TST_BIN_FILES=$(TST_MODULES:%=$(TST_BIN_DIR)/%.beam)
EXM_FILES=$(wildcard $(EXM_DIR)/*.erl)
TMP_FILES=$(TMP_PATTERN) $(addprefix $(APP_SRC_DIR)/, $(TMP_PATTERN)) $(addprefix $(HDR_DIR)/, $(TMP_PATTERN)) $(addprefix $(DOC_DIR)/, $(TMP_PATTERN)) $(addprefix $(TST_SRC_DIR)/, $(TMP_PATTERN)) $(addprefix $(EXM_DIR)/, $(TMP_PATTERN))

ENTER_ERL=erl -noinput -pa $(APP_BIN_DIR) -eval '
EXIT_ERL=' -run init stop
ERLC=erlc
APP_ERLC_FLAGS=-W2 -Ddebug +debug_info +warn_missing_spec +warn_untyped_record -I $(HDR_DIR)
TST_ERLC_FLAGS=-W2 +debug_info +nowarn_unused_function -I $(HDR_DIR)
EDOC_OPTIONS=[{dialyzer_specs,all}, {report_missing_type,true}, {report_type_mismatch,true}, {pretty_print,erl_pp}, {preprocess,true}]
EUNIT_OPTIONS=[]
DIALYZER=dialyzer
DIALYZER_FLAGS=-Wunmatched_returns
NEEDED_APPS=compiler erts kernel stdlib crypto
RM=rm -f
TAR=tar -czf


# TODO: separate debug and optimization options
# TODO: extra targets: test, tags, commit/update
# TODO: header and text files as dependencies: more fine-grained

.PHONY: default all compile tests doc test check clean distclean rebuild release build_plt

default: compile

all: compile doc

compile: $(APP_BIN_FILES)

$(APP_BIN_FILES): $(HDR_FILES)

$(APP_BIN_DIR)/%.beam: $(APP_SRC_DIR)/%.erl
	$(ERLC) $(APP_ERLC_FLAGS) -o $(APP_BIN_DIR) $<

tests: $(TST_BIN_FILES)
	$(ENTER_ERL) eunit:test({dir,"$(TST_BIN_DIR)"},$(EUNIT_OPTIONS)) $(EXIT_ERL)

$(TST_BIN_FILES): $(HDR_FILES)

$(TST_BIN_DIR)/%.beam: $(TST_SRC_DIR)/%.erl
	$(ERLC) $(TST_ERLC_FLAGS) -o $(TST_BIN_DIR) $<

doc: $(APP_SRC_FILES) $(HDR_FILES) $(TXT_FILES)
	$(ENTER_ERL) edoc:application(proper, ".", $(EDOC_OPTIONS)) $(EXIT_ERL)

check: compile
	$(DIALYZER) $(DIALYZER_FLAGS) $(APP_BIN_FILES)

clean:
	@echo removing temporary files...
	@$(RM) $(TMP_FILES)

distclean: clean
	@echo removing build artifacts...
	@$(RM) $(APP_BIN_FILES) $(DOC_FILES) $(TST_BIN_FILES) $(RELEASE_FILE)

rebuild: distclean compile

release: all clean
	$(RM) $(RELEASE_FILE)
	$(TAR) $(RELEASE_FILE) $(APP_SRC_FILES) $(APP_BIN_FILES) $(HDR_FILES) $(DOC_FILES) $(TST_SRC_FILES) $(EXM_FILES) $(TXT_FILES)

build_plt:
	$(DIALYZER) --build_plt --apps $(NEEDED_APPS)
