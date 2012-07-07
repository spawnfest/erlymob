APPLICATION := erlymob

ERL := erl
EPATH := -pa apps/*/ebin -pz deps/*/ebin
TEST_EPATH := -pa .eunit -pz deps/*/ebin

.PHONY: all compile deps doc clean depclean distclean dialyze test console test-console

all: compile

compile:
	@rebar compile

deps:
	@rebar get-deps

doc:
	@rebar skip_deps=true doc

clean:
	@rebar skip_deps=true clean

depclean:
	@rebar clean

distclean:
	@rebar delete-deps

dialyze: compile
	@dialyzer -r ./apps/

test:
	@rebar skip_deps=true eunit

console:
	$(ERL) -sname $(APPLICATION) $(EPATH) -config app

test-console: test
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH) -config app

