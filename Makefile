.PHONY: default all compile tests doc check clean distclean rebuild retest

default: compile

all: compile doc

compile:
	./rebar compile

tests:
	./rebar eunit

doc:
	./rebar doc

check: compile
	./rebar dialyze

clean:
	./clean_temp.sh

distclean: clean
	./rebar clean

rebuild:
	./rebar clean
	./rebar compile

retest:
	rm -rf .eunit
	./rebar eunit
