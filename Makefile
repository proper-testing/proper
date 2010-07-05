APP_SRC_DIR=src
APP_BIN_DIR=ebin
HDR_DIR=include
DOC_DIR=doc
DOC_PATTERN=index.html overview-summary.html modules-frame.html packages-frame.html stylesheet.css erlang.png edoc-info
TST_SRC_DIR=tests
TST_BIN_DIR=tests
EXM_DIR=examples
UTIL_SRC_DIR=util
UTIL_BIN_DIR=util
TMP_PATTERN=*~ \\\#*\\\# *.dump

TXT_FILES=COPYING README
DOC_SRC_FILES=$(DOC_DIR)/overview.edoc
MAKE_FILES=Makefile vsn.mk $(APP_SRC_DIR)/proper.app.src
PACKAGE_FILE=proper.tar.gz

include vsn.mk

APP_SRC_FILES=$(wildcard $(APP_SRC_DIR)/*.erl)
APP_MODULES=$(APP_SRC_FILES:$(APP_SRC_DIR)/%.erl=%)
APP_BIN_FILES=$(APP_MODULES:%=$(APP_BIN_DIR)/%.beam) $(APP_BIN_DIR)/proper.app
HDR_FILES=$(wildcard $(HDR_DIR)/*.hrl)
DOC_FILES=$(addprefix $(DOC_DIR)/, $(DOC_PATTERN) $(addsuffix .html, $(APP_MODULES)))
TST_SRC_FILES=$(wildcard $(TST_SRC_DIR)/*.erl)
TST_MODULES=$(TST_SRC_FILES:$(TST_SRC_DIR)/%.erl=%)
TST_BIN_FILES=$(TST_MODULES:%=$(TST_BIN_DIR)/%.beam)
EXM_FILES=$(wildcard $(EXM_DIR)/*.erl)
UTIL_SRC_FILES=$(wildcard $(UTIL_SRC_DIR)/*.erl)
UTIL_MODULES=$(UTIL_SRC_FILES:$(UTIL_SRC_DIR)/%.erl=%)
UTIL_BIN_FILES=$(UTIL_MODULES:%=$(UTIL_BIN_DIR)/%.beam)
TMP_FILES=$(TMP_PATTERN) $(addprefix $(APP_SRC_DIR)/, $(TMP_PATTERN)) $(addprefix $(HDR_DIR)/, $(TMP_PATTERN)) $(addprefix $(DOC_DIR)/, $(TMP_PATTERN)) $(addprefix $(TST_SRC_DIR)/, $(TMP_PATTERN)) $(addprefix $(EXM_DIR)/, $(TMP_PATTERN)) $(addprefix $(UTIL_SRC_DIR)/, $(TMP_PATTERN))
BUILD_FILES=$(APP_BIN_FILES) $(DOC_FILES) $(TST_BIN_FILES) $(UTIL_BIN_FILES) $(PACKAGE_FILE)

ENTER_ERL=erl -noinput -pa $(APP_BIN_DIR) -eval '
EXIT_ERL=' -run init stop
ERLC=erlc
ERLC_FLAGS=-W2 +debug_info -I $(HDR_DIR)
ifdef NOTYPES
APP_ERLC_FLAGS=-pa $(UTIL_BIN_DIR) +{parse_transform,strip_types} -o $(APP_BIN_DIR)
else
APP_ERLC_FLAGS=+warn_missing_spec +warn_untyped_record -o $(APP_BIN_DIR)
endif
TST_ERLC_FLAGS=+nowarn_unused_function -o $(TST_BIN_DIR)
UTIL_ERLC_FLAGS=-o $(UTIL_BIN_DIR)
EDOC_OPTIONS=[{dialyzer_specs,all}, {report_missing_type,true}, {report_type_mismatch,true}, {pretty_print,erl_pp}, {preprocess,true}]
EUNIT_OPTIONS=[]
DIALYZER=dialyzer
DIALYZER_FLAGS=-Wunmatched_returns
NEEDED_APPS=compiler erts kernel stdlib crypto
RM=rm -f
TAR=tar -czf


# TODO: separate debug and optimization options
# TODO: extra targets: tags, commit/update, check_plt, analyze
# TODO: header files as dependencies: more fine-grained
# TODO: fill in the modules and applications in the .app file on the fly

.PHONY: default all compile tests util doc check clean distclean rebuild package build_plt

default: compile

all: compile doc

compile: util $(APP_BIN_FILES)

$(APP_BIN_FILES): $(HDR_FILES) $(MAKE_FILES)

$(APP_BIN_DIR)/%.beam: $(APP_SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) $(APP_ERLC_FLAGS) $<

$(APP_BIN_DIR)/%.app: $(APP_SRC_DIR)/%.app.src
	sed -e s^%PROPER_VSN%^$(PROPER_VSN)^ $< > $@

tests: compile $(TST_BIN_FILES)
	$(ENTER_ERL) eunit:test({dir,"$(TST_BIN_DIR)"},$(EUNIT_OPTIONS)) $(EXIT_ERL)

$(TST_BIN_FILES): $(HDR_FILES) $(MAKE_FILES)

$(TST_BIN_DIR)/%.beam: $(TST_SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) $(TST_ERLC_FLAGS) $<

util: $(UTIL_BIN_FILES)

$(UTIL_BIN_DIR)/%.beam: $(UTIL_SRC_DIR)/%.erl $(MAKE_FILES)
	$(ERLC) $(ERLC_FLAGS) $(UTIL_ERLC_FLAGS) $<

doc: $(DOC_SRC_FILES) $(APP_SRC_FILES) $(HDR_FILES) $(MAKE_FILES)
	$(ENTER_ERL) edoc:application(proper, ".", $(EDOC_OPTIONS)) $(EXIT_ERL)

check: compile
	$(DIALYZER) $(DIALYZER_FLAGS) $(APP_BIN_DIR)

clean:
	@echo -n removing temporary files...
	@$(RM) $(TMP_FILES)
	@echo done

distclean: clean
	@echo -n removing build artifacts...
	@$(RM) $(BUILD_FILES)
	@echo done

rebuild: distclean compile

package: all clean
	$(RM) $(PACKAGE_FILE)
	@echo -n packaging...
	@$(TAR) $(PACKAGE_FILE) $(APP_SRC_FILES) $(APP_BIN_FILES) $(HDR_FILES) $(DOC_SRC_FILES) $(DOC_FILES) $(TST_SRC_FILES) $(UTIL_SRC_FILES) $(UTIL_BIN_FILES) $(EXM_FILES) $(TXT_FILES) $(MAKE_FILES)
	@echo done

build_plt:
	$(DIALYZER) --build_plt --apps $(NEEDED_APPS)
