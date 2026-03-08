ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all compile test local-test local-eunit clean clean_doc doc docker-build docker-test

REBAR3 ?= rebar3
DOCKER_IMAGE ?= mcc-test:latest

all: compile

compile:
	$(REBAR3) compile

test: docker-test

docker-build:
	docker build -t $(DOCKER_IMAGE) -f Dockerfile.test .

docker-test: docker-build
	docker run -t --rm -v $(PWD):/app -w /app $(DOCKER_IMAGE) make local-test

local-test: local-eunit

local-eunit:
	$(REBAR3) eunit

clean: clean_doc
	$(REBAR3) clean

clean_doc:
	rm -rf doc

doc: clean_doc
	$(REBAR3) edoc
