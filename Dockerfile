FROM rocker/r-ver:devel

RUN apt-get update && apt-get install -y \
	    sudo \
	        gdebi-core \
		    pandoc \
		        pandoc-citeproc \
			    libcurl4-gnutls-dev \
			        libcairo2-dev \
				    libxt-dev \
				        wget

# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
	    VERSION=$(cat version.txt)  && \
	        wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
		    gdebi -n ss-latest.deb && \
		        rm -f version.txt ss-latest.deb && \
			    . /etc/environment && \
			        R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://mran.microsoft.com/')" && \
				    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

RUN R -e "install.packages(c('devtools', 'plotly', 'DT', 'broom', 'dplyr', 'tidyr', 'BH'), repos='https://mran.microsoft.com/')" && \
	R -e "devtools::install_github('hadley/ggplot2')" && \
	R -e "devtools::install_github('rstudio/shinydashboard')" && \
	R -e "devtools::install_github('dloewenstein/ewavesPDFshiny')"

COPY /inst/shinyApp /srv/shiny-server/
COPY /inst/shinyApp/shiny-server.sh /usr/bin/shiny-server.sh
COPY /inst/shinyApp/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Port to expose
EXPOSE 3839

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
