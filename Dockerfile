FROM rocker/rstudio
MAINTAINER Ben Neely <nigelneely@gmail.com>

RUN R -e "install.packages(c('shiny'), repos='https://cran.rstudio.com/')"
