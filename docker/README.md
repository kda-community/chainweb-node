# Quick Setup

1.  *(Skip this step if you run the Chainweb node in data center.)* Log into your
    router and configure port forwarding for port 1789 to your computer.

2.  Make sure that your firewall allows incoming connection on port 1789.

3.  Initialize database *(optional but saves several hours of db synchronization
    on node startup.)*:

    First you need a database snapshot URL. See below, how to obtain a database
    snapshot.

    ```sh
    docker run -ti --rm -e DBURL=YOUR_DB_SNAPSHOT_URL -v chainweb-data:/data kadena/chainweb-node /chainweb/initialize-db.sh
    ```

4.  Start Chainweb node:

    ```sh
    docker run -d -p 1789:1789 -p 1848:1848 -v chainweb-data:/data kadena/chainweb-node
    ```

For explanations and additional configuration options (like, for instance, using
an alternate port) keep reading.

# Minimal System Requirements

*   CPU: 2 cores
*   RAM: 4 GB
*   Storage: 150 GB of free space (it is recommend to use SSD disks)
*   Network: public IP address or port forwarding

For instance AWS EC2 t3a.medium VMs with 150GB SSD root storage are known to work.
Fast HDD disks are fine to, but should provide good IOPS values.

# Running a Chainweb Node as Docker Container

A Chainweb node serves two separate APIs:

1.  The P2P API includes all endpoints that are used for node-to-node
    communication. It is served via HTTPS and must be reachable from the public
    internet.

2.  The Service API includes all routes that off Kadena Chainweb services to
    users and applications. It is served as plain (unencrypted) HTTP and can
    kept private. It is also possible to use this API with a reverse proxy.

More details about these APIs can be found further down in this document.

**A Chainweb node must be reachable from the public internet**. It needs a
public IP address and port. If you run the node from a data center, usually, you
only have to ensure that it can be reached on the default P2P port *1789*. You can use
the following shell command to start the node.

```sh
docker run -d -p 1848:1848 -p 1789:1789 kadena/chainweb-node
```

This exposes the P2P network on HTTPS port 1789 and the API services of Chainweb
node on HTTP port 1848.

If you are running the node from a local network with NAT (network address
translation), which is the case for most home networks, you'll have to configure
port forwarding for the P2P port (1789) in your router.

Using different ports is possible, too. For the P2P API the internal and external
port of the docker container must match and the internal P2P port can be changed
using the `CHAINWEB_P2P_PORT` environment variable. For instance, the following
command exposes the P2P network on port 443.

```sh
docker run -d -p 1848:80 -p 443:443 -e "CHAINWEB_P2P_PORT=443" kadena/chainweb-node
```

More options to configure the node are described at the bottom of this document.

Above command starts the node with an empty Chainweb database. It will take
about 2-3 days until the node has "caught up" with the network, joined
consensus, and can be used for mining and processing transactions.

The startup time can be shortened to a few minutes by initializing the node with
a pre-computed database. This is strongly encouraged and described further down
in this document.

# Initialize Chainweb Database

When the container is started for the first time it has to synchronize and
rebuild the Chainweb database from the P2P network. This can take a long time.
Currently, as of 2021-02-17, this takes about 2-3 days for a node in a well
connected data center.

The container includes a script for synchronizing a pre-build database, which
currently, as of 2021-02-17, involves downloading about 15GB of data.

A database snapshot is just a gzipped tar archive of the Chainweb database,
which contains the subdirectories `rocksDb` and `sqlite`. The URL can point to
remote location or a local file. Any URL that curl understands is fine.

Database snapshots are available from different sources. Kadena offers an
up-to-date snapshot at
https://kadena-node-db.s3.us-east-2.amazonaws.com/db-chainweb-node-ubuntu.18.04-latest.tar.gz.
This file is stored in a request-pays S3 bucket. In order to access it you need
an AWS account and you must create and signed URL for authenticating with S3.
Details about how to do this can be found here:
https://docs.aws.amazon.com/AmazonS3/latest/userguide/ObjectsinRequesterPaysBuckets.html

With `node.js` you can create a signed URL for above snapshot URL as follows:

```js
// get-chainweb-image-url.js
AWS = require("aws-sdk");
const s3 = new AWS.S3({
  accessKeyId: AWS_ACCESS_KEY_ID, // Add your Access Key ID from IAM
  secretAccessKey: AWS_SECRET_ACCESS_KEY, // Add your Secret Access Key from IAM
  region: "us-east-2"
})
const params = {
  Bucket: 'kadena-node-db',
  Expires: 3600,
  Key: 'db-chainweb-node-ubuntu.18.04-latest.tar.gz',
  RequestPayer: 'requester'
}
// When ran, the script will output exclusively the signed url
s3.getSignedUrl("getObject", params, (_err, res) => console.log(res))
```

With Python one can use the following code:

```python
import boto3
client = boto3.client('s3')
url  = client.generate_presigned_url(
    "get_object",
    Params = {
        "Bucket":"kadena-node-db",
        "Key":"db-chainweb-node-ubuntu.18.04-latest.tar.gz",
        "RequestPayer":'requester'
    }
)
```

### Database within Chainweb node container

The following shell commands initializes a docker container with a database and
creates a new image from it.

```sh
npm install aws-sdk
YOUR_DB_SNAPSHOT_URL=$(node get-chainweb-image-url.js) # assuming you use get-chainweb-image-url.js form above
docker run -ti --name initialize-chainweb-db -e DBURL=$YOUR_DB_SNAPSHOT_URL kadena/chainweb-node /chainweb/initialize-db.sh
docker commit `docker ps -a -f 'name=initialize-chainweb-db' -q` chainweb-node-with-db
docker rm initialize-chainweb-db
```

Once the database is initialized the image can be used to run a Chainweb node as
follows:

```sh
docker run \
    --detach \
    --publish 1848:1848 \
    --publish 1789:1789 \
    --name chainweb-node \
    chainweb-node-with-db \
    /chainweb/run-chainweb-node.sh
```

# Persistent the Chainweb database (outside of container)

When the Chainweb database is stored in the container it is lost when the
container is deleted. This could, for instance, happen when a new version of
Chainweb is released and the node is upgraded to the new version.

It is therefore recommended to store the Chainweb database outside the container
on a docker volume (preferred method) or in the file system of the host system.

```sh
# 1. Get signed database snapshot URL (assuming you use get-chainweb-image-url.js form above)
npm install aws-sdk
YOUR_DB_SNAPSHOT_URL=$(node get-chainweb-image-url.js)

# 2. Initialize a database that is persisted on a docker volume
docker run -ti --rm \
    --mount type=volume,source=chainweb-data,target=/data \
    --env DBURL=$YOUR_DB_SNAPSHOT_URL \
    kadena/chainweb-node \
    /chainweb/initialize-db.sh

# 3. Use the database volume with a Chainweb node
docker run \
    --detach \
    --publish 1848:1848 \
    --publish 1789:1789 \
    --name chainweb-node \
    --mount type=volume,source=chainweb-data,target=/data \
    kadena/chainweb-node
```

Alternatively a bind mount can be used to persist the database in the file
system of the host.

# Enable Mining API

The private mining API is enabled by providing the public key of a miner as
environment variable in the container using the `-e` option of Docker's `run`
command. Optionally, the miner account name can be given, too. If the account
name is omitted the public key is used as account name.

Only a single miner with a single is supported. The key predicate is `keys-all`.

The following example provides a public miner key and an account name:

```sh
docker run \
    --detach \
    --publish 1848:1848 \
    --publish 1789:1789 \
    --env "MINER_KEY=26a9285cd8db34702cfef27a5339179b5a26373f03dd94e2096b0b3ba6c417da" \
    --env "MINER_ACCOUNT=merle" \
    --name chainweb-node \
    --mount type=volume,source=chainweb-data,target=/data \
    kadena/chainweb-node
```

Please refer to the official
[chainweb-mining-client](https://github.com/kadena-io/chainweb-mining-client)
for a further information of how to use the mining API to mine on Chainweb. The
official reference implementation does not support mining devices that are
powerful enough to competitively mine on the Kadena Mainnet. Links to alternate
mining software can be found
[here](https://kadena-io.github.io/kadena-docs/Public-Chain-Docs/#start-mining).

# API Overview

TODO

# Verifying database consistency

TODO

# Running the test suite

TODO

# Technical Details

This Docker image runs a chainweb-node using the public IP address of the host
with self-signed certificates. It supports a single mining key. For many use
cases this is sufficient.

For more complex production setups it is recommended to clone the repository,
edit the configuration file within the Docker container, and create an custom
image.

When a public domain name is used instead of a domain name, we recommend to use
docker compose to run [certbot]() in a different container and share the
certificates using docker volumes.

### Commands:

* `/chainweb/run-chainweb-node.sh`
* `/chainweb/initialize-db.sh`
* `/chainweb/check-reachability.sh`
* `/chainweb/check-health.sh`

### File System

*   Database directory: `/data/chainweb-db`
*   Chainweb configuration file: `/chainweb/chainweb.mainnet01.yaml`

### Available Configuration Options

The following environment variables can be used to configured the image using
the `--env` option of `docker run`.

*   `CHAINWEB_P2P_PORT`: the network port that is used by the Chainweb P2P
    network. The port that is used internally in the container *must* match the
    port that is used publicly. A appropriate port mapping must be passed to the
    `docker run` command, e.g. `-p 443:443`. (default: `1789`)

*   `CHAINWEB_SERVICE_PORT`: the network port that is used by the Chainweb
    REST Service API. Usually, there is no need to change the default value,
    because the internal port can be mapped to some external port by passing a
    port mapping to the `docker run` command, e.g. `-p 1848:80`. (default:
    `1848`)

*   `LOGLEVEL`: the log-level that is used by the Chainweb node.
    Possible  values are `quiet`, `error`, `warn`, `info`, and `debug`.
    The value `debug` should be avoid during normal production.
    (default: `warn`).

*   `CHAINWEB_P2P_HOST`: the public IP address of the node. (default: automatically
    detected)

*   `MINER_KEY`: the public key of the miner. If this is empty or unset
    mining is disabled. Only a single key is supported with key predicate
    `keys-all`.

*   `MINER_ACCOUNT`: the mining account for the miner key. If unset or empty
    the `MINER_KEY` is also used as account name.

*   `ROSETTA`: Any non-empty value enables the
    [Rosetta API](https://www.rosetta-api.org/) of the Chainweb node (default: disabled).

*   TODO:
    *    running a node with a DNS domain name
    *    option for setting the block gas limit
    *    option to enable the header stream
    *    explain how to overwrite the configuration file

Options for `/chainweb/initialize-db.sh`

*   `DBURL`: The URL from where the database snapshot is downloaded. We
    recommend that users maintain there own database snapshots.

Here is an example for how to use these settings:

```sh
docker run \
    --detach \
    --publish 8000:80 \
    --publish 1789:1789 \
    --name chainweb-node \
    --env "CHAINWEB_P2P_PORT=1789" \
    --env "CHAINWEB_SERVICE_PORT=8000" \
    --env "LOGLEVEL=warn" \
    --env "MINER_KEY=774723b442c6ee660a0ac34747374fcd451591a123b35df5f4a69f1e9cb2cc75" \
    --env "MINER_ACCOUNT=merle" \
    --env "ROSETTA=1" \
    --mount type=volume,source=chainweb-data,target=/data \
    kadena/chainweb-node
```

### API Endpoints

P2P API endpoints (HTTPS, inter-node communication):

* `^/chainweb/0.0/mainnet01/cut`
* `^/chainweb/0.0/mainnet01/chain/[0-9]+/(header|hash|branch|payload)`
* `^/chainweb/0.0/mainnet01/chain/[0-9]+/mempool`

Service API endpoints (HTTP):

* `^/info`
* `^/health-check`
* `^/chainweb/0.0/mainnet01/chain/[0-9]+/pact/`
* `^/chainweb/0.0/mainnet01/rosetta/` (optional, off by default)
* `^/chainweb/0.0/mainnet01/header/updates` (optional, off by default)
* `^/chainweb/0.0/mainnet01/mining/` (optional, off by default)

Only the endpoints that are required for inter-node communication are included
in the P2P API. The port of the P2P must be publicly available.

All endpoints that are not used for inter-node communication are included in to
the service API. It is up to the node operator to decide if and how to make the
service API available to users.

A detailed documentation of the API is available at https://api.chainweb.com.

