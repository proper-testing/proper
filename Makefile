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

ERL = $(shell which erl)
ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif
 
REBAR=$(shell which rebar || ./rebar)
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

DIALYZER_FLAGS=$(shell ./dialyzer_flags.sh)

.PHONY: fast all get-deps compile run-dialyzer check-escripts tests doc clean distclean rebuild retest

default: fast .dialyzer.plt dialyzer

fast: get-deps compile

all: default doc tests

include/compile_flags.hrl:
	./write_compile_flags $@

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

.dialyzer.plt:
	dialyzer --build_plt \
		--output_plt .dialyzer.plt \
		$(DIALYZER_FLAGS) \
		--apps erts kernel stdlib sasl \
			compiler crypto tools runtime_tools \
			mnesia inets ssl public_key asn1 \
			edoc eunit syntax_tools xmerl

run-dialyzer:
	dialyzer \
		--plt .dialyzer.plt \
		$(DIALYZER_FLAGS) \
		ebin $(find .  -path 'deps/*/ebin/*.beam')

dialyzer: .dialyzer.plt compile run-dialyzer

check-escripts:
	./check_escripts.sh make_doc write_compile_flags

tests: compile
	@$(REBAR) eunit

doc:
	./make_doc

pdf:
	pandoc README.md -o README.pdf

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
