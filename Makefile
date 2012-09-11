all: clean
	./rebar get-deps
	./rebar update-deps
	./rebar compile

clean: clean_doc
	./rebar clean
	./rebar delete-deps

test: clean
	./rebar eunit skip_deps=true

clean_doc:
	rm -rf doc
doc: clean_doc
	./rebar doc