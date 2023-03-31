FROM rocker/shiny:4.2.1

ENV SHINY_LOG_STDERR 1

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2 \
    libudunits2-dev \
    libproj-dev \
    libgdal-dev

RUN R -e 'install.packages(c(\
        "shiny",\
        "plotly",\
        "leaflet",\
        "leaflet.extras",\
        "plyr",\
        "dplyr",\
        "rgdal",\
        "shinyWidgets",\
        "rnaturalearth",\
        "tidyverse",\
        "sf",\
        "rgeos",\
        "httr",\
        "jsonlite",\
        "raster",\
        "data.table",\
        "DT",\
        "leafem",\
        "RColorBrewer",\
        "shinydisconnect",\
        "shinyjs",\
        "leaflet.multiopacity"\
    ),\
    repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2022-09-02"\
)'

COPY ./app/dev/* /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
