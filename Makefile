# Copyright 2010-2013 Manolis Papadakis <manopapad@gmail.com>,
#                     Eirini Arvaniti <eirinibob@gmail.com>
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

# Author(s):   Manolis Papadakis, Kostis Sagonas
# Description: Instructions for make

REBAR=`which rebar || ./rebar`
REBAR_PLT_DIR?=.

.PHONY: fast all get-deps compile dialyzer check_escripts tests doc clean distclean rebuild retest

default: fast $(REBAR_PLT_DIR)/.dialyzer_plt dialyzer

fast: get-deps compile

all: default doc tests

include/compile_flags.hrl:
	./write_compile_flags $@

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

$(REBAR_PLT_DIR)/.dialyzer_plt:
	dialyzer --build_plt \
		--output_plt $(REBAR_PLT_DIR)/.dialyzer_plt \
		--apps erts kernel stdlib sasl \
			compiler crypto tools runtime_tools \
			mnesia inets ssl public_key asn1 \
			edoc eunit syntax_tools xmerl \
	| fgrep -v -f ./dialyzer.build.ignore-warnings

dialyzer: compile
	dialyzer \
		--plt $(REBAR_PLT_DIR)/.dialyzer_plt \
		-n -nn \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wunderspecs \
		ebin $(find .  -path 'deps/*/ebin/*.beam')

check_escripts:
	./check_escripts.sh make_doc write_compile_flags

tests: compile
	@$(REBAR) eunit

doc:
	./make_doc

clean:
	./clean_temp.sh

distclean: clean
	rm -f include/compile_flags.hrl
	@$(REBAR) clean

rebuild: distclean include/compile_flags.hrl
	@$(REBAR) compile

retest: compile
	rm -rf .eunit
	@$(REBAR) eunit
