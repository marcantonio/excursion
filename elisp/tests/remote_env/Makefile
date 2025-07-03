SHELL       := /usr/bin/env bash
USER        := user1
BIN	    := excursion
REMOTE_HOME := remote_home
OWNER       := $(shell id -u):$(shell id -g)
MOUNT_PATH  := /home/$(USER)
REPO_ROOT   := $(shell git -C $(CURDIR) rev-parse --show-toplevel)
FWD_PORT    := 17001

.PHONY: env-key env-cargo-build env-up env-down env-ssh

env-key:
	@[[ -f $(USER)_key ]] || ssh-keygen -t ed25519 -N "" -f $(USER)_key

env-cargo-build:
	cargo build --manifest-path $(REPO_ROOT)/Cargo.toml
	mkdir -p $(REMOTE_HOME)
	cp $(REPO_ROOT)/target/debug/$(BIN) $(REMOTE_HOME)

env-up: env-key env-cargo-build
	BIN_NAME=$(BIN) docker compose up -d --build
	ssh -fNT -i $(USER)_key -p 2222 -L$(FWD_PORT):localhost:7001 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no $(USER)@localhost

env-down:
	- docker compose exec excursion-remote chown -R $(OWNER) $(MOUNT_PATH) 2>/dev/null || true
	docker compose down
