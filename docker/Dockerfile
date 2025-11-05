# syntax=docker/dockerfile:experimental

# Run as
#
# --ulimit nofile=64000:64000

# BUILD PARAMTERS
ARG UBUNTUVER=22.04

FROM ubuntu:${UBUNTUVER}

ARG REVISION=d7aee70
ARG GHCVER=9.8.2
ARG UBUNTUVER
ARG STRIP=1

LABEL revision="$REVISION"
LABEL ghc="$GHCVER"
LABEL ubuntu="$UBUNTUVER"

# install prerequisites
RUN apt-get update \
    && apt-get install -y curl xxd openssl binutils libtbb2 libgflags2.2 libsnappy1v5 locales libmpfr6 \
    && rm -rf /var/lib/apt/lists/* \
    && locale-gen en_US.UTF-8 \
    && update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

ENV LANG=en_US.UTF-8

# Install chainweb applications
WORKDIR /chainweb
RUN curl -Ls "https://community-kda-cabal-cache.s3.amazonaws.com/chainweb-node/chainweb.true.${GHCVER}.ubuntu-${UBUNTUVER}.${REVISION}.tar.gz" \
    | tar -xzC "/" \
        chainweb/chainweb-node \
        chainweb/LICENSE \
        chainweb/README.md \
        chainweb/CHANGELOG.md \
    && { [ $STRIP -eq 1 ] && strip chainweb-node ; }

# Install scripts
COPY run-chainweb-node.sh .
COPY initialize-db.sh .
COPY chainweb.mainnet01.yaml .
COPY chainweb.testnet04.yaml .
COPY chainweb.development.yaml .
COPY check-health.sh .
RUN chmod 755 run-chainweb-node.sh initialize-db.sh check-health.sh

# Create Database directories
RUN mkdir -p /data/chainweb-db \
    mkdir -p /root/.local/share/chainweb-node/mainnet01/ \
    mkdir -p /root/.local/share/chainweb-node/testnet04/ \
    mkdir -p /root/.local/share/chainweb-node/development/

# Command
STOPSIGNAL SIGTERM
HEALTHCHECK --start-period=5m --interval=1m --retries=5 --timeout=10s CMD ./check-health.sh

CMD ./run-chainweb-node.sh
