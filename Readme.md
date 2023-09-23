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
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:latest
```

Open in browser: `localhost:3838/`

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
docker tag local-image:tagname new-repo:tagname
docker tag network-stats-shiny:latest crtahlin/swarm-network-stats:0.34

# Push
docker login -u crtahlin
docker push crtahlin/swarm-network-stats:0.34
# and / or just latest
docker tag network-stats-shiny:latest crtahlin/swarm-network-stats:latest
docker push crtahlin/swarm-network-stats:latest

# Test
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:0.34
localhost:3838/
docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:latest
localhost:3838/
``` 

