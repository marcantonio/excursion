EMACS ?= emacs

TEST_DIR=$(shell pwd)/tests

MATCH ?=

.PHONY: test

test:
	$(EMACS) --batch -L . -L $(TEST_DIR) -l excursion-tests.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'
