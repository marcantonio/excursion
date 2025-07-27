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

/usr/sbin/sshd -D &

# Changes for file-attributes tests
touch -d "2025-07-22 01:01:01" "$HOME_DIR/test_root/file-attributes/dir1"
chmod 775 "$HOME_DIR/test_root/file-attributes/dir1"
touch -d "2025-07-22 01:01:01" "$HOME_DIR/test_root/file-attributes/foo"
chmod 664 "$HOME_DIR/test_root/file-attributes/foo"
touch -h -d "2025-07-23 01:01:01" "$HOME_DIR/test_root/file-attributes/bar"

exec su - "$USER" -c "exec /usr/local/bin/$BIN"
