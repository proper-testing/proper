# Copyright 2010-2019 Manolis Papadakis <manopapad@gmail.com>,
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

# Author(s):   Manolis Papadakis and Kostis Sagonas
# Description: Instructions for make

.PHONY: default all compile dialyzer check_escripts test doc clean distclean rebuild retest

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

PROPER_REBAR := .$(SEP)rebar

default: compile

all: compile dialyzer doc test

compile:
	$(PROPER_REBAR) compile

dialyzer: .plt/proper_plt compile
	dialyzer -n -nn --plt $< -Wunmatched_returns ebin

.plt/proper_plt: .plt
	dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib compiler crypto syntax_tools eunit

check_escripts:
	./check_escripts.sh make_doc

test:
	$(PROPER_REBAR) eunit

doc: compile
	./make_doc

clean:
	./clean_temp.sh

distclean: clean
	$(RM) -r .eunit .rebar
	$(RM) .plt/proper_plt
	$(PROPER_REBAR) clean

rebuild: distclean compile

retest:
	$(RM) -r .eunit
	$(PROPER_REBAR) eunit
