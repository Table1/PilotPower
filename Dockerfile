FROM rocker/shiny:3.4.4

MAINTAINER Erik Westlund

RUN  echo 'install.packages(c("pwr"), \
repos="http://cran.us.r-project.org", \
dependencies=TRUE)' > /tmp/packages.R \
  && Rscript /tmp/packages.R

EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]