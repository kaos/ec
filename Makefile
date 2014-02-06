# Options for erlang.mk
PROJECT = ec

DEPS = merl
TEST_DEPS ?= proper

dep_merl = https://github.com/kaos/merl.git master
dep_proper = https://github.com/manopapad/proper.git master

CT_SUITES = proper

include erlang.mk

# erlang.mk bootstrapping
erlang.mk: erlang_mk_url ?= \
	http://raw.github.com/extend/erlang.mk/master/erlang.mk

erlang.mk:
	@echo " GET   " $@; wget -O $@ $(erlang_mk_url)

check: TEST_DEPS=
check: DEPS=
check: tests

proper: TEST_DEPS=
proper: DEPS=
proper: clean deps app build-tests
	erl -pa ebin $(DEPS_DIR)/*/ebin test -noshell \
		-eval '[proper:F(ec_props) || F <- [check_specs, module]]' \
		-s init stop

.PHONY: check proper
