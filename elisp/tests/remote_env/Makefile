SHELL       := /usr/bin/env bash
USER        := user1
BIN	    := excursion
REMOTE_HOME := remote_home
OWNER       := $(shell id -u):$(shell id -g)
MOUNT_PATH  := /home/$(USER)
FWD_PORT    := 17001

.PHONY: env-key env-up env-down env-bounce env-ssh

env-key:
	@[[ -f $(USER)_key ]] || ssh-keygen -t ed25519 -N "" -f $(USER)_key

env-up: env-key
	BIN_NAME=$(BIN) docker compose up -d --build
	ssh -fNT -i $(USER)_key -p 2222 -L$(FWD_PORT):localhost:7001 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no $(USER)@localhost

env-down:
	- docker compose exec excursion-remote chown -R $(OWNER) $(MOUNT_PATH) 2>/dev/null || true
	docker compose down

env-bounce: env-down env-up
