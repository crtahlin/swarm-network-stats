# Description

An app using the [Shiny](https://www.shinyapps.io/) platform on top of [R](https://cran.r-project.org/), reading data from [swarmscan.io](https://swarmscan.io/) and rendering various statistics on screen.

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
docker pull crtahlin/swarm-network-stats
docker run --rm -p 3838:3838 crtahlin/network-stats-shiny:latest
```

Open in browser: `localhost:3838/`

# Instructions to push to dockerhub

```
# Build
docker build -t network-stats-shiny .

# Test
docker run --rm -p 3838:3838 network-stats-shiny:latest

# Tag
docker tag local-image:tagname new-repo:tagname
docker tag network-stats-shiny:latest crtahlin/swarm-network-stats:0.32

# Push
docker login -u crtahlin
docker push crtahlin/swarm-network-stats:0.32

# Test
# docker run --rm -p 3838:3838 crtahlin/network-stats-shiny:latest
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:0.32
localhost:3838/
``` 

