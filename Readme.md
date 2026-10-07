# Description

An app using the [Shiny](https://www.shinyapps.io/) platform on top of [R](https://cran.r-project.org/), reading data from [swarmscan.io](https://swarmscan.io/) and from the Gnosis chain, and rendering various statistics on screen.

# Data sources

- **swarmscan.io**: the network dump (`https://api.swarmscan.io/v1/network/dump`), downloaded every 10 minutes.
- **Gnosis chain** (`R/chain.R`): stake per node, redistribution reveals and the storage price, read from the [storage-incentives](https://github.com/ethersphere/storage-incentives) contracts over JSON-RPC every 10 minutes. The first read takes a few seconds; later reads fetch only new blocks. Reveals and price changes are kept for the last 30 days. The app uses the public endpoint `https://rpc.gnosischain.com`; set the environment variable `SWARM_RPC_URL` for the R process to use another one.

- **Storage history** (`data/storage-history.csv`): one row a day from swarmscan's archive of daily network dumps (`https://swarmscan.sos-ch-dk-2.exo.io/network/dumps/`), starting 2024-01-13, for the Storage growth tab. Each row has the node counts, the most common storage radius, reserve fullness and the stored-data estimate. Extend it with `Rscript scripts/build_storage_history.R`, which fetches only the days not yet in the file; a full build downloads about 1,000 files of 5 to 40 MB and takes about an hour. Dumps before bee reported `reserveSizeWithinRadius` only have `reserveSize`, which overstates stored data; those rows say `whole reserve` in the `measure` column, are drawn in grey and are left out of the growth fits. A node with reserve doubling d reports storageRadius = committedDepth − d and the reserve of the 2^d neighbourhoods it stores, so its stored-data estimate needs no correction, and its fullness is taken against its capacity of 2^(22+d) chunks; dumps before December 2024 have no `committedDepth`. When the 00:00 dump lacks the nodes' status, the 06:00, 12:00 and 18:00 dumps of that day are tried; days with fewer than 100 reporting nodes are left out of the plots.

# Disclaimer

The app is meant to be informative in nature, no guarantees are made about correctness of displayed statistics.

# Instructions

## Building docker image localy
```
git clone https://github.com/crtahlin/swarm-network-stats.git
cd swarm-network-stats
docker build -t network-stats-shiny .
docker run --rm -p 3838:3838 network-stats-shiny:latest
```

Open in browser: `localhost:3838/`


## Runing image from dockerhub (might not be latest code)
```
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:latest
```

Open in browser: `localhost:3838/`

## Troubleshooting

The image is built for AMD/Intel architectures, so if you have an ARM Mac, go to the settings in your Docker Desktop and set to use Rosetta emulation. Otherwise it does not seem to work. Probably does not work on other ARM systems.

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

```
# Build
docker build -t network-stats-shiny .

# Test
docker run --rm -p 3838:3838 network-stats-shiny:latest
localhost:3838/

# Tag
# docker tag local-image:tagname new-repo:tagname
docker tag network-stats-shiny:latest crtahlin/swarm-network-stats:0.36
docker tag network-stats-shiny:latest crtahlin/swarm-network-stats:latest

# Push
docker login -u crtahlin
docker push crtahlin/swarm-network-stats:0.36
# and / or just latest
docker push crtahlin/swarm-network-stats:latest

# Test
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:0.36
localhost:3838/
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:latest
localhost:3838/
``` 

