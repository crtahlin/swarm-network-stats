docker build -t network-stats-shiny .

docker run --rm -p 3838:3838 network-stats-shiny:latest

docker tag local-image:tagname new-repo:tagname
docker push new-repo:tagname

docker tag network-stats-shiny:latest crtahlin/swarm-network-stats:0.32
docker login -u crtahlin
docker push crtahlin/swarm-network-stats:0.32

# docker run --rm -p 3838:3838 crtahlin/network-stats-shiny:latest

docker run --rm -p 3838:3838 crtahlin/swarm-network-stats:0.32
localhost:3838/
