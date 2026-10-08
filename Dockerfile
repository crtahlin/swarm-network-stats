# rocker/r-ver is published for amd64 and arm64, and each version tag installs packages from a dated
# Posit Package Manager snapshot (4.5.2: 2026-03-10), so two builds of one commit get the same
# package versions. The app runs with shiny::runApp, as shiny-server would run it (one R process
# for the app, plus the background reads it starts); shiny-server itself is published for amd64
# only, and it does not pass `docker run -e` variables (such as SWARM_RPC_URL) on to the app
FROM rocker/r-ver:4.5.2

# tini forwards docker stop's signal to R, which ignores it as process 1; the libraries are what
# leaflet's imports sf and raster need at run time (Ubuntu 24.04 names)
RUN apt-get update && apt-get install -y --no-install-recommends \
      tini libudunits2-0 libgdal34t64 libgeos-c1t64 libproj25 \
    && rm -rf /var/lib/apt/lists/*

# every package app.R and R/ load; httr, jsonlite, htmltools, htmlwidgets and callr are used as pkg::
RUN install2.r --error --skipinstalled \
      shiny bslib DT leaflet ggplot2 dplyr forstringr httr jsonlite htmltools htmlwidgets callr remotes \
    && rm -rf /tmp/downloaded_packages
# SwarmR (first_n_places) from GitHub, pinned to a commit
RUN Rscript -e 'remotes::install_github("crtahlin/SwarmR@c2c660b85d638876c143e7ef13a3b698eecaa68a", upgrade = "never")'

RUN useradd --create-home --uid 1000 app
WORKDIR /app
COPY app.R ./app.R
COPY R ./R
COPY data ./data
USER app

EXPOSE 3838
ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
