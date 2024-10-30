# Rocker Shiny alap
FROM rocker/shiny:latest

# shiny-app könyvtár
RUN mkdir /home/shiny-app

WORKDIR /home/shiny-app

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

 
# R csomagok telepítése
RUN R -e "install.packages(c('sf','plotly', 'shiny', 'ggplot2', 'dplyr', 'data.table', 'scales', 'DT', 'thematic', 'vctrs', 'writexl', 'R.utils', 'stringi'), repos='https://cloud.r-project.org/')"

# Másolás
COPY  . /home/shiny-app/

# port meghatározása
EXPOSE 3838

ENV OPENSSL_CONF=openssl.cnf

# Program futtatása 
CMD ["R", "-e", "shiny::runApp('app.R',  host = '0.0.0.0', port = 3838)"]
