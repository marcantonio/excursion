#!/usr/bin/env bash
set -e

USER=user1
HOME_DIR=/home/$USER
BIN="${BIN_NAME:-}"

chown -R user1:user1 "$HOME_DIR"

if [[ -f /tmp/${USER}_key.pub ]]; then
  install -o "$USER" -g "$USER" -m 0700 -d "$HOME_DIR/.ssh"
  install -o "$USER" -g "$USER" -m 0600 /tmp/${USER}_key.pub "$HOME_DIR/.ssh/authorized_keys"
fi

# TEST_ROOT="$HOME_DIR/test_root"
# mkdir $TEST_ROOT

/usr/sbin/sshd -D &

exec su - "$USER" -c "exec $HOME_DIR/$BIN"
