FROM ubuntu:24.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        openssh-server ca-certificates && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir /var/run/sshd && echo "PasswordAuthentication no" >> /etc/ssh/sshd_config

RUN useradd -m -s /bin/bash emacsuser

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

EXPOSE 22

ENTRYPOINT ["/entrypoint.sh"]
