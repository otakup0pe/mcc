ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all compile test test-local local-compile local-eunit local-ct local-dialyzer \
        dialyzer shell clean distclean docker-build docker-test

REBAR3 ?= rebar3
COMPOSE ?= docker compose -f docker-compose.test.yml

all: compile

compile:
	$(REBAR3) compile

test: docker-test

docker-build:
	$(COMPOSE) build

docker-test: docker-build
	$(COMPOSE) run --rm test

test-local: local-eunit local-dialyzer
	$(REBAR3) cover

local-compile:
	$(REBAR3) compile

local-eunit:
	$(REBAR3) eunit

local-ct:
	$(REBAR3) ct

local-dialyzer:
	$(REBAR3) dialyzer

dialyzer:
	$(REBAR3) dialyzer

shell:
	$(REBAR3) shell

clean:
	$(REBAR3) clean

distclean: clean
	rm -rf _build
	$(COMPOSE) down -v
