FROM rocker/shiny:latest

# https://stackoverflow.com/questions/75717174/how-do-you-install-a-package-from-github-in-docker-for-an-r-shiny-app
# Install required libraries -- using prebuild binaries where available
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev

# RUN R -e "install.packages('devtools', repos='http://cloud.r-project.org')"

RUN R -e 'install.packages(c("shiny","shinyWidgets","ggplot2","lubridate", "remotes"))'
RUN R -e 'install.packages(c("DT", "leaflet","forstringr", "DescTools"))'
RUN R -e 'remotes::install_github("crtahlin/SwarmR")'

#RUN install.r remotes

#RUN installGithub.r crtahlin/SwarmR

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/

COPY ./app.R ./app.R
