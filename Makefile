all: clean
	rebar3 get-deps

clean: clean_doc
	rebar3 clean

test: clean
	rebar3 eunit skip_deps=true

clean_doc:
	rm -rf doc

doc: clean_doc
	rebar3 doc
