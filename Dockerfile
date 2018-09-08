FROM rocker/shiny

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    wget


RUN R -e "install.packages(c('devtools', 'plotly'), repos='https://cloud.r-project.org/')" && \
    R -e "devtools::install_github('hadley/ggplot2')" && \
    R -e "devtools::install_github('jrowen/rhandsontable')" && \
    R -e "devtools::install_github('rstudio/shinydashboard')"

ADD /echoewaves_app /srv/shiny-server/

EXPOSE 3838

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
