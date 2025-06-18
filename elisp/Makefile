EMACS         := emacs
TEST_DIR      := $(shell pwd)/tests
MATCH         ?=
REMOTE_DIR    := tests/remote
TEST_ENDPOINT := localhost:17001

.PHONY: test test-only test-%

test: test-env-tunnel
	echo "### Running tests (endpoint = $(TEST_ENDPOINT))"; \
	TEST_ENDPOINT=$(TEST_ENDPOINT) \
	$(EMACS) --batch -L . -L $(TEST_DIR) \
	         --eval "(setq excursion-test-endpoint \"$(TEST_ENDPOINT)\")" \
	         -l excursion-tests.el \
		 --eval '(ert-run-tests-batch-and-exit "$(MATCH)")'; \
	status=$$?; \
	$(MAKE) test-env-down; \
	exit $$status

test-only:
	echo "### Running tests (endpoint = $(TEST_ENDPOINT))"; \
	TEST_ENDPOINT=$(TEST_ENDPOINT) \
	$(EMACS) --batch -L . -L $(TEST_DIR) \
	         --eval "(setq excursion-test-endpoint \"$(TEST_ENDPOINT)\")" \
	         -l excursion-tests.el \
		 --eval '(ert-run-tests-batch-and-exit "$(MATCH)")';


test-%:
	$(MAKE) -C $(REMOTE_DIR) $*
