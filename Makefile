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

.PHONY: default fast all get-deps compile dialyzer check_escripts tests doc clean distclean rebuild retest

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

REBAR := .$(SEP)rebar

default: fast dialyzer

fast: get-deps compile

all: default doc tests

include/compile_flags.hrl:
	./write_compile_flags $@

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

dialyzer: compile
	dialyzer -n -nn -Wunmatched_returns ebin $(find .  -path 'deps/*/ebin/*.beam')

check_escripts:
	./check_escripts.sh make_doc write_compile_flags

tests: compile
	$(REBAR) eunit

doc:
	./make_doc

clean:
	./clean_temp.sh

distclean: clean
	rm -f include/compile_flags.hrl
	$(REBAR) clean

rebuild: distclean include/compile_flags.hrl
	$(REBAR) compile

retest: compile
	rm -rf .eunit
	$(REBAR) eunit
