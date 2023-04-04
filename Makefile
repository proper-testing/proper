# Copyright 2010-2023 Manolis Papadakis <manopapad@gmail.com>,
#                     Eirini Arvaniti <eirinibob@gmail.com>,
#                 and Kostis Sagonas <kostis@cs.ntua.gr>
#
# This file is part of PropEr.
#
# PropEr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PropEr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

# Author(s):   Manolis Papadakis and Kostis Sagonas
# Description: Instructions for make

.PHONY: default all compile dialyzer check_escripts test test-examples test-parallel doc clean distclean rebuild retest

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

# A safe version of rebar3 that still supports Erlang/OTP 20.x and 21.x
# Upgrade to a newer version when support for these versions is dropped.
REBAR3_URL := https://github.com/erlang/rebar3/releases/download/3.15.2/rebar3
REBAR3 ?= $(shell which rebar3 || which .$(SEP)rebar3 || \
            (wget --no-check-certificate $(REBAR3_URL) && \
	      chmod +x rebar3 && echo .$(SEP)rebar3))
COVER ?= false

default: compile

all: compile dialyzer doc test #test-examples -- this is another GitHub action

compile:
	$(RM) ebin
	$(REBAR3) compile
	ln -s _build/default/lib/proper/ebin .

dialyzer: .plt/proper_plt compile
	dialyzer --plt $< -Wunmatched_returns ebin

.plt/proper_plt: .plt
	dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib compiler crypto syntax_tools mnesia tools runtime_tools

check_escripts:
	./scripts/check_escripts.sh make_doc

test: compile
ifeq ($(COVER), true)
	$(REBAR3) do eunit -c, cover, covertool generate
else
	$(REBAR3) eunit
endif

test-examples:
	$(REBAR3) eunit --dir=examples --verbose

test-parallel:
	NUMWORKERS=2 $(REBAR3) eunit --dir=examples --verbose

doc: compile
	./scripts/make_doc

clean:
	./scripts/clean_temp.sh
	./scripts/clean_doc.sh
	$(REBAR3) clean

distclean: clean
	$(RM) ebin .plt/proper_plt
	$(RM) rebar3 rebar.lock
	$(RM) -r _build

rebuild: distclean compile

retest:
	$(RM) -r _build/test/lib/proper/test
	$(REBAR3) eunit
