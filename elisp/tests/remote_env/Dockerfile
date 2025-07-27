FROM rust:1.88 AS builder
WORKDIR /usr/local/excursion

COPY Cargo.toml Cargo.lock ./
COPY src ./src

RUN cargo build

FROM ubuntu:24.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        openssh-server ca-certificates && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir /var/run/sshd

RUN useradd -m -s /bin/bash user1

COPY --from=builder /usr/local/excursion/target/debug/excursion /usr/local/bin/excursion

COPY elisp/tests/remote_env/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

EXPOSE 22

ENTRYPOINT ["/entrypoint.sh"]
