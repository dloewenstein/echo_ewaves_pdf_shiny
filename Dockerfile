FROM rocker/shiny

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
				        wget

RUN R -e "install.packages(c('devtools', 'shiny', 'plotly', 'DT', 'broom', 'dplyr', 'tidyr', 'BH'), repos='https://mran.microsoft.com/')" && \
	R -e "devtools::install_github('hadley/ggplot2')" && \
	R -e "devtools::install_github('rstudio/shinydashboard')"

COPY /inst/shinyApp /srv/shiny-server/
COPY /inst/shinyApp/shiny-server.sh /usr/bin/shiny-server.sh
COPY /inst/shinyApp/shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN R -e "devtools::install_github('dloewenstein/ewavesPDFshiny')" # redo
# Add custom nginx template
ADD nginx.conf.sigil /app/nginx.conf.sigil

# Port to expose
EXPOSE 3839

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
