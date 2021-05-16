FROM rocker/geospatial:latest
LABEL maintainer="justin@justinsingh.me"

ENV S6_VERSION=v1.21.7.0
ENV SHINY_SERVER_VERSION=latest
ENV PANDOC_VERSION=default

RUN /rocker_scripts/install_shiny_server.sh

# Install Shiny Dependencies
RUN install2.r -e -s \
    flexdashboard \
    bslib \
    htmltools \
    httr

# Install Spatial Dependencies
RUN install2.r -e -s \
    leaflet \
    dataRetrieval

## Note: most likely will remove these once API is functional ##
RUN Rscript -e "remotes::install_github('ropensci/USAboundaries')"
RUN Rscript -e "remotes::install_github('ropensci/USAboundariesData')"
RUN Rscript -e "remotes::install_github('mikejohnson51/climateR')"
################################################################

# Install Data Processing Dependencies
RUN install2.r -e -s \
    gt \
    janitor \
    formattable

RUN Rscript -e "remotes::install_github('ianmoran11/mmtable2')"

# Copy Shiny App to Container
COPY . /srv/shiny-server

# Open Shiny Port
EXPOSE 3838

CMD ["/init"]
