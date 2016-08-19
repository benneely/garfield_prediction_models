FROM rocker/rstudio
MAINTAINER Ben Neely <nigelneely@gmail.com>

RUN apt-get update && apt-get install -y \
	libxml2-dev

RUN R -e "install.packages(c('XML','shiny','ggplot2','scales'), repos='https://cran.rstudio.com/')"

EXPOSE 3838
