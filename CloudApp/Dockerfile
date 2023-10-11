# get shiny server plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    curl git libboost-all-dev \
    libgdal-dev libgit2-dev libproj-dev libssl-dev  \
    libudunits2-0 libudunits2-dev \
    git-core \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# install R packages required 
# (change it depending on the packages you need)
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rgdal', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sp', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('leaflet.extras')"
RUN R -e "install.packages('leaflet.esri')"
RUN R -e "install.packages('raster')"
RUN R -e "install.packages('htmlwidgets')"


# clean up
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# Copy shiny app into the Docker image
COPY app /srv/shiny-server/

RUN rm /srv/shiny-server/index.html

# Make the ShinyApp available at port 5000
EXPOSE 5000

# Copy shiny app execution file into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

USER shiny

CMD ["/usr/bin/shiny-server"]
