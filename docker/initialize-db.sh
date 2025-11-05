#!/usr/bin/env bash

if [[ -z "$DBURL" ]] ; then
    echo "Please provide an URL for a database snapshot using '-e DBURL=<URL>'"
    exit 1
fi

DBDIR="/data/chainweb-db"

CHAINWEB_NETWORK=${CHAINWEB_NETWORK:-mainnet01}

# Install database

if [[ "$CHAINWEB_NETWORK" = "mainnet01" ]] ; then
    mkdir -p "$DBDIR/0" && \
    curl "$DBURL" | tar -xzC "$DBDIR/0"
fi

