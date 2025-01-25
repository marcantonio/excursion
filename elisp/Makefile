EMACS ?= emacs

TEST_DIR=$(shell pwd)/tests

MATCH ?=

.PHONY: test

test:
	$(EMACS) --batch -L . -L $(TEST_DIR) -l all.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'
