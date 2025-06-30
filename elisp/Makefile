EMACS         := emacs
MATCH         ?=
TEST_DIR      := $(shell pwd)/tests
REMOTE_DIR    := $(TEST_DIR)/remote_env
TEST_ENDPOINT := localhost:17001

.PHONY: test test-only test-%

test: test-env-up
	echo "### Running tests (endpoint = $(TEST_ENDPOINT))"; \
	TEST_ENDPOINT=$(TEST_ENDPOINT) \
	$(EMACS) --batch -L . -L $(TEST_DIR) \
	         -l excursion-tests.el \
		 --eval '(ert-run-tests-batch-and-exit "$(MATCH)")'; \
	status=$$?; \
	$(MAKE) test-env-down; \
	exit $$status

test-only:
	echo "### Running tests (endpoint = $(TEST_ENDPOINT))"; \
	TEST_ENDPOINT=$(TEST_ENDPOINT) \
	$(EMACS) --batch -L . -L $(TEST_DIR) \
	         -l excursion-tests.el \
		 --eval '(ert-run-tests-batch-and-exit "$(MATCH)")';


test-%:
	$(MAKE) -C $(REMOTE_DIR) $*
