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


RUN R -e "install.packages(c('DT'), repos='$MRAN')"

ADD /echoewaves_app /srv/shiny-server/

EXPOSE 3838

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]