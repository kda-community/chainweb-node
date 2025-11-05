#!/usr/bin/env bash

# ############################################################################ #
# PARAMETERS

export CHAINWEB_NETWORK=${CHAINWEB_NETWORK:-mainnet01}
export CHAINWEB_P2P_PORT=${CHAINWEB_P2P_PORT:-1789}
export CHAINWEB_SERVICE_PORT=${CHAINWEB_SERVICE_PORT:-1848}
export LOGLEVEL=${LOGLEVEL:-warn}
export MINER_KEY=${MINER_KEY:-}
export MINER_ACCOUNT=${MINER_ACCOUNT:-$MINER_KEY}
export SKIP_REACHABILITY_CHECK=${SKIP_REACHABILITY_CHECK:-0}

if [[ -z "$CHAINWEB_P2P_HOST" ]] ; then
    CHAINWEB_P2P_HOST="0.0.0.0"
fi
export CHAINWEB_P2P_HOST

# ############################################################################ #
# Check ulimit

UL=$(ulimit -n -S)
[[ "$UL" -ge 65536 ]] ||
{
    echo "The configuration of the container has a too tight limit for the number of open file descriptors. The limit is $UL but at least 65536 is required." 1>&2
    echo "Try starting the container with '--ulimit \"nofile=65536:65536\"'" 1>&2
    exit 1
}

# ############################################################################ #
# Create chainweb database directory
#
# The database location is configured in chainweb.yaml

DBDIR="/data/chainweb-db"
mkdir -p "$DBDIR/0"

# ############################################################################ #
# Configure Miner

if [[ -z "$MINER_KEY" ]] ; then
export MINER_CONFIG="
chainweb:
  mining:
    coordination:
      enabled: ${MINING_ENABLED:-false}
"
else
export MINER_CONFIG="
chainweb:
  mining:
    coordination:
      enabled: true
      miners:
        - account: $MINER_ACCOUNT
          public-keys: [ $MINER_KEY ]
          predicate: keys-all
"
fi

# ############################################################################ #
# Run node

exec ./chainweb-node \
    --config-file="chainweb.${CHAINWEB_NETWORK}.yaml" \
    --config-file <(echo "$MINER_CONFIG") \
    --p2p-hostname="$CHAINWEB_P2P_HOST" \
    --p2p-port="$CHAINWEB_P2P_PORT" \
    --service-port="$CHAINWEB_SERVICE_PORT" \
    --log-level="$LOGLEVEL" \
    +RTS -N -t -A64M -H500M -RTS \
    "$@"

