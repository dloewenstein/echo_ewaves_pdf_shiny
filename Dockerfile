FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
	    sudo \
	        gdebi-core \
		    pandoc \
		        pandoc-citeproc \
			    libcurl4-gnutls-dev \
			        libcairo2-dev \
				    libxt-dev \
				    libssl-dev \
				    libgit2-dev \
                    libxml2-dev \
				        wget

RUN R -e "install.packages(c('devtools', 'shiny', 'plotly', 'DT', 'broom', 'dplyr', 'tidyr'), repos='https://mran.microsoft.com/')" && \
	R -e "devtools::install_github('hadley/ggplot2')" && \
	R -e "devtools::install_github('rstudio/shinydashboard')"

RUN R -e "install.packages('rclipboard')"

COPY /inst/shinyApp /srv/shiny-server/
COPY /inst/shinyApp/shiny-server.sh /usr/bin/shiny-server.sh
COPY /inst/shinyApp/shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN R -e "devtools::install_github('dloewenstein/ewavesPDFshiny', ref = 'development')"
# Add custom nginx template
#WORKDIR /app
#ADD nginx.conf.sigil /app/nginx.conf.sigil

# Port to expose
EXPOSE 3839

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
