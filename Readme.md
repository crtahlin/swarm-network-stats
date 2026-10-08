# Description

An app using the [Shiny](https://www.shinyapps.io/) platform on top of [R](https://cran.r-project.org/), reading data from [swarmscan.io](https://swarmscan.io/) and from the Gnosis chain, and rendering various statistics on screen.

# Data sources

- **swarmscan.io**: the network dump (`https://api.swarmscan.io/v1/network/dump`), downloaded every 10 minutes.
- **Gnosis chain** (`R/chain.R`): stake per node, redistribution reveals and the storage price, read from the [storage-incentives](https://github.com/ethersphere/storage-incentives) contracts over JSON-RPC every 10 minutes. The first read takes a few seconds; later reads fetch only new blocks. Reveals and price changes are kept for the last 30 days. The app uses the public endpoint `https://rpc.gnosischain.com`; set the environment variable `SWARM_RPC_URL` for the R process to use another one. No log query spans more than 1,000,000 blocks, because over 50,000 results the public endpoint can answer with an empty list instead of an error; for an endpoint with a lower limit, set a smaller span with `SWARM_RPC_LOG_SPAN`.

Both reads run in a separate background R process (`R/background.R`, with the `callr` package), so a read never stalls the sessions: they keep showing the last good data until the new data arrives, and see a "Reading data" message until the first read is done. A read that runs longer than 3 minutes is stopped and counted as failed.

- **Storage history** (`data/storage-history.csv`): one row a day from swarmscan's archive of daily network dumps (`https://swarmscan.sos-ch-dk-2.exo.io/network/dumps/`), starting 2024-01-13, for the Storage growth tab. Each row has the node counts, the most common storage radius, reserve fullness and the stored-data estimate. Extend it with `Rscript scripts/build_storage_history.R`, which fetches only the days not yet in the file; a full build downloads about 1,000 files of 5 to 40 MB and takes about an hour. Dumps before bee reported `reserveSizeWithinRadius` only have `reserveSize`, which overstates stored data; those rows say `whole reserve` in the `measure` column, are drawn in grey and are left out of the growth fits. A node with reserve doubling d reports storageRadius = committedDepth − d and the reserve of the 2^d neighbourhoods it stores, so its stored-data estimate needs no correction, and its fullness is taken against its capacity of 2^(22+d) chunks; dumps before December 2024 have no `committedDepth`. When the 00:00 dump lacks the nodes' status, the 06:00, 12:00 and 18:00 dumps of that day are tried; days with fewer than 100 reporting nodes are left out of the plots.

# Disclaimer

The app is meant to be informative in nature, no guarantees are made about correctness of displayed statistics.

# Instructions

## Building the docker image locally
```
git clone https://github.com/crtahlin/swarm-network-stats.git
cd swarm-network-stats
docker build -t network-stats-shiny .
docker run --rm -p 3838:3838 network-stats-shiny:latest
```

Open in browser: `localhost:3838/`. The first data arrives within about a minute; until then the sidebar says it is reading.

The image is based on `rocker/r-ver:4.5.2`, which is published for amd64 and arm64 and installs R packages from a dated Posit Package Manager snapshot (2026-03-10), so the build works natively on ARM Macs and two builds of one commit get the same package versions. SwarmR is pinned to a commit. The app runs with `shiny::runApp` in the container's only R process.

To use another Gnosis RPC endpoint, pass it to the container:
```
docker run --rm -p 3838:3838 -e SWARM_RPC_URL=https://your.endpoint network-stats-shiny:latest
```

## Running the image from dockerhub (might not be latest code)
```
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:latest
```

Open in browser: `localhost:3838/`

# Tests

Run the regression test from the repository root:

```
Rscript tests/test_outputs.R
```

It loads `app.R`, feeds it the saved sample in `tests/fixtures/swarmscan-sample.json` instead of downloading from swarmscan, answers chain requests from `tests/fixtures/chain-sample.json` through `tests/fake_rpc.R`, and runs every output with `shiny::testServer`. It checks the values against counts computed directly from the data, also on copies of the sample with fields removed, and the data refresh with a failing download. It prints the number of passed and failed checks and exits with status 1 on any failure. It takes about 20 seconds.

The chain sample is 6 hours of reveals, claims and price updates before Gnosis block 48,632,531 (2026-10-07), plus the stake history and current stake of 134 owners. The decoded values in the test were checked against foundry's `cast`.

The swarmscan sample is 492 nodes from swarmscan's dump of 2026-09-30, reduced to the fields the app reads, with public IP addresses replaced by addresses from the 198.18.0.0/15 benchmarking range.

# Contributions

Fork, improve, do a PR. No promises about response times. Thank you.

# Instructions to push to dockerhub (to self)

Build for both architectures and push in one step (needs `docker login -u crtahlin` first):

```
docker buildx build --platform linux/amd64,linux/arm64 \
  -t crtahlin/swarm-network-stats:0.41 -t crtahlin/swarm-network-stats:latest --push .

# Test
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:0.41
localhost:3838/
```
