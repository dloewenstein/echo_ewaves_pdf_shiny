FROM rocker/shiny

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    wget \
    libssl-dev \
    libgit2-dev


RUN R -e "install.packages(c('devtools', 'plotly', 'DT', 'broom', 'dplyr', 'tidyr', 'shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')" && \
    R -e "devtools::install_github('hadley/ggplot2')" && \
    R -e "devtools::install_github('rstudio/shinydashboard')"

Run R -e "devtools::install_github('dloewenstein/ewavesPDFshiny')"

COPY /inst/shinyApp /srv/shiny-server/
COPY /inst/shinyApp/shiny-server.sh /usr/bin/shiny-server.sh
COPY /inst/shinyApp/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Port to expose
EXPOSE 3838

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
